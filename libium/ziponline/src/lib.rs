//! `ziponline` is a minimalist ZIP file extractor that uses HTTP range requests to avoid extracting the entire archive when you only need to extract a few individual files.
//!
//! There are two main ways to extract zip files:
//! - `ziponline::extract_file` - extracts a singular file from the archive.
//! - `ziponline::LazyZipFile` - uses caching to speed-up multiple file extractions from the same archive.
//!
//! # Examples
//!
//! Extract a single file
//! ```rust
//! let mut reader = ziponline::extract_file(
//!     &client,    // the reqwest::Client
//!     &url,       // the url of the zip file
//!     None,       // the filesize or None if it is unknown
//!     "file.txt", // the name of the file to extract
//! ).await?;
//!
//! // `reader` is an io::Read object.
//! io::copy(&mut reader, ...);
//! ```
//!
//! Extract multiple files
//! ```rust
//! let mut zip_file = LazyZipFile::new(
//!     client,   // the reqwest::Client
//!     url,      // the url of the zip file
//!     filesize, // the filesize or None if it is unknown
//! ).await?;
//!
//! let abc_reader = zip_file.extract_file("abc.txt").await?;
//! let def_reader = zip_file.extract_file("def.txt").await?;
//! let ghi_reader = zip_file.extract_file("ghi.txt").await?;
//! ```

#![warn(clippy::cargo)]
#![warn(missing_docs)]
#![allow(clippy::let_unit_value)]
use std::{collections::HashMap, io, num::ParseIntError, sync::Arc};

use bytes::Buf;
use reqwest::{Client, IntoUrl, Url, header::ToStrError};

use crate::{
    parser::{BoxParserStream, Parser, ParserStream},
    structs::{Cdfh, CompressionMethod, Eocd, Eocd32, Eocd64},
};

mod parser;
mod structs;

/// Zip file that fetches the data it needs lazily.
/// If you plan to extract only one file from the zip use [`extract_file`] instead.
pub struct LazyZipFile {
    client: Client,
    url: Url,
    headers: HashMap<String, Arc<Cdfh>>,
    cd: CdParser<BoxParserStream>,
}

impl LazyZipFile {
    /// Create a new lazy zip file with the reqwest::Client and filesize.
    pub async fn new<U>(client: Client, url: U, filesize: Option<usize>) -> Result<Self>
    where
        U: IntoUrl,
    {
        Self::new_inner(client, url.into_url()?, filesize).await
    }

    /// Create a new lazy zip file, assuming the filesize is unknown.
    pub async fn get<U>(url: U) -> Result<Self>
    where
        U: IntoUrl,
    {
        Self::new_inner(Client::new(), url.into_url()?, None).await
    }

    async fn new_inner(client: Client, url: Url, filesize: Option<usize>) -> Result<Self> {
        let filesize = match filesize {
            Some(filesize) => filesize,
            None => request_content_length(&client, &url).await?,
        };

        let Some(eocd) = request_eocd(&client, &url, filesize).await? else {
            return Err(Error::EocdNotFound);
        };

        let cd = request_cd(&client, &url, &eocd).await?;

        Ok(Self {
            cd: cd.into_box(),
            headers: HashMap::new(),
            client,
            url,
        })
    }

    /// Extract a single file from the zip.
    /// Returns an io::Read to the raw contents of the file.
    pub async fn extract_file(&mut self, filename: &str) -> Result<impl io::Read> {
        let cdfh = match self.headers.get(filename) {
            Some(cdfh) => Some(Arc::clone(cdfh)),
            None => loop {
                let Some(cdfh) = self.cd.next().await? else {
                    break None;
                };

                let cdfh = Arc::new(cdfh);
                self.headers
                    .insert(cdfh.filename.clone(), Arc::clone(&cdfh));
                if cdfh.filename == filename {
                    break Some(cdfh);
                }
            },
        };

        match cdfh {
            Some(cdfh) => read_file_at_cdfh(&self.client, &self.url, &cdfh).await,
            None => Err(Error::CdFileNotFound),
        }
    }
}

/// Extract a single file from a zip.
/// If you plan to extract multiple files from the same zip use [`LazyZipFile`] instead.
pub async fn extract_file(
    client: &Client,
    url: &Url,
    filesize: Option<usize>,
    filename: &str,
) -> Result<impl io::Read> {
    let filesize = match filesize {
        Some(filesize) => filesize,
        None => request_content_length(client, url).await?,
    };

    let Some(eocd) = request_eocd(client, url, filesize).await? else {
        return Err(Error::EocdNotFound);
    };

    let Some(cdfh) = find_cdfh(client, url, &eocd, filename).await? else {
        return Err(Error::CdFileNotFound);
    };

    read_file_at_cdfh(client, url, &cdfh).await
}

/// Read the file contents at the file header offset in the given CDFH.
async fn read_file_at_cdfh(
    client: &Client,
    url: &Url,
    cdfh: &Cdfh,
) -> Result<impl io::Read + use<>> {
    let resp = client
        .get(url.clone())
        .header("Range", format!("bytes={}-", cdfh.file_header_offset))
        .send()
        .await?
        .error_for_status()?;

    let mut parser = Parser::new(resp.bytes_stream());
    let signature: [u8; 4] = parser.take_fixed_slice().await?;
    if signature != *b"PK\x03\x04" {
        return Err(Error::MalformedFileHeader);
    }

    let Some(()) = read_fh(&mut parser).await? else {
        return Err(Error::MalformedFileHeader);
    };

    let bytes = parser.take_bytes(cdfh.compressed_size as usize).await?;
    let reader = inflate::DeflateDecoder::new(bytes.reader());

    Ok(reader)
}

/// Find a file inside the central directory.
async fn find_cdfh(
    client: &Client,
    url: &Url,
    eocd: &Eocd,
    filename: &str,
) -> Result<Option<Cdfh>> {
    let mut parser = request_cd(client, url, eocd).await?;

    while let Some(cdfh) = parser.next().await? {
        if cdfh.filename == filename {
            return Ok(Some(cdfh));
        }
    }

    Ok(None)
}

/// Make a range request for the Central Directory and pass the response data to `CdParser`.
async fn request_cd(
    client: &Client,
    url: &Url,
    eocd: &Eocd,
) -> Result<CdParser<impl ParserStream + use<>>> {
    let range = format!("bytes={}-{}", eocd.cd_offset, eocd.cd_offset + eocd.cd_size);

    let resp = client
        .get(url.clone())
        .header("Range", range)
        .send()
        .await?
        .error_for_status()?;

    let parser = Parser::new(resp.bytes_stream());

    Ok(CdParser::new(parser, eocd.offset))
}

/// Parses the central directory and provides an API similar to Iterator but for Central Directory File Headers.
struct CdParser<S: ParserStream> {
    parser: Parser<S>,
    maximum_allowed_offset: usize,
}

impl<S: ParserStream> CdParser<S> {
    fn new(parser: Parser<S>, maximum_allowed_offset: usize) -> Self {
        Self {
            parser,
            maximum_allowed_offset,
        }
    }

    fn into_box(self) -> CdParser<BoxParserStream>
    where
        S: 'static,
    {
        CdParser {
            parser: self.parser.into_box(),
            maximum_allowed_offset: self.maximum_allowed_offset,
        }
    }

    async fn next(&mut self) -> Result<Option<Cdfh>> {
        let mut peek_buf: [u8; 4] = [0; 4];

        while let Some(value) = self.parser.next().await? {
            peek_buf.rotate_left(1);
            peek_buf[peek_buf.len() - 1] = value;

            // Is it a CDFH?
            if peek_buf == *b"PK\x01\x02" {
                let cdfh = match read_cdfh(&mut self.parser, self.maximum_allowed_offset).await {
                    Ok(None) | Err(Error::UnexpectedEof) => continue,
                    Ok(Some(cdfh)) => cdfh,
                    Err(e) => return Err(e),
                };

                return Ok(Some(cdfh));
            }
        }

        Ok(None)
    }
}

/// Read a central directory file header or None if it is a false positive.
/// The given reader should return bytes right after the magic number `PK\x01\x02`.
async fn read_cdfh<S: ParserStream>(
    r: &mut Parser<S>,
    maximum_allowed_offset: usize,
) -> Result<Option<Cdfh>> {
    let version_made_by = r.take_u16().await?;
    let version_to_extract = r.take_u16().await?;
    // The version is stored in the last 8 bits of the field,
    // if the version is larger than 63 it's likely a false positive.
    if (version_made_by & 0xff) > 63 || (version_to_extract & 0xff) > 63 {
        return Ok(None);
    }
    let general_purpose_flags = r.take_u16().await?;
    let compression_method_id = r.take_u16().await?;
    let Some(compression_method) = CompressionMethod::from_id(compression_method_id) else {
        return Ok(None);
    };
    let last_modification_time = r.take_u16().await?;
    let last_modification_date = r.take_u16().await?;
    let crc32 = r.take_u32().await?;
    let compressed_size = r.take_u32().await?;
    let uncompressed_size = r.take_u32().await?;
    let filename_length = r.take_u16().await?;
    let extra_field_length = r.take_u16().await?;
    let file_comment_length = r.take_u16().await?;
    let disk_number = r.take_u16().await?;
    let internal_attrs = r.take_u16().await?;
    let external_attrs = r.take_u32().await?;
    let file_header_offset = r.take_u32().await?;
    if file_header_offset as usize > maximum_allowed_offset {
        return Ok(None);
    }

    // Filename should be valid UTF-8
    let Ok(filename) = String::from_utf8(r.take_bytes(filename_length as usize).await?.to_vec())
    else {
        return Ok(None);
    };
    let _extra_field = r.skip_bytes(extra_field_length as usize).await?;
    let _file_comment = r.skip_bytes(file_comment_length as usize).await?;

    Ok(Some(Cdfh {
        version_made_by,
        version_to_extract,
        general_purpose_flags,
        compression_method,
        last_modification_time,
        last_modification_date,
        crc32,
        compressed_size,
        uncompressed_size,
        extra_field_length,
        file_comment_length,
        disk_number,
        internal_attrs,
        external_attrs,
        file_header_offset,
        filename,
    }))
}

/// Read a file header or None if it is a false positive.
/// The given reader should return bytes right after the magic number `PK\x03\x04`.
async fn read_fh<S: ParserStream>(r: &mut Parser<S>) -> Result<Option<()>> {
    let version_to_extract = r.take_u16().await?;
    // The version is stored in the last 8 bits of the field,
    // if the version is larger than 63 it's likely a false positive.
    if (version_to_extract & 0xff) > 63 {
        return Ok(None);
    }

    let _general_purpose_flags = r.take_u16().await?;
    let compression_method_id = r.take_u16().await?;
    let Some(compression_method) = CompressionMethod::from_id(compression_method_id) else {
        return Ok(None);
    };
    assert_eq!(
        compression_method,
        CompressionMethod::Deflated,
        "only DEFLATE compression is supported"
    );

    let _last_modification_time = r.take_u16().await?;
    let _last_modification_date = r.take_u16().await?;

    let _crc32 = r.take_u32().await?;
    let _compressed_size = r.take_u32().await?;
    let _uncompressed_size = r.take_u32().await?;

    let filename_length = r.take_u16().await?;
    let extra_field_length = r.take_u16().await?;

    let _filename = r.skip_bytes(filename_length as usize).await?;
    let _extra_field = r.skip_bytes(extra_field_length as usize).await?;

    Ok(Some(()))
}

/// Find the EOCD header.
/// Currently only checks the last 256 bytes of the file,
/// so if the EOCD is larger than 256 bytes it won't be found.
// TODO: implement handling of eocd larger than 256 bytes
async fn request_eocd(client: &Client, url: &Url, filesize: usize) -> Result<Option<Eocd>> {
    const CHUNK_SIZE: usize = 256;

    let from = filesize - CHUNK_SIZE;
    let to = filesize - 1;

    let resp = client
        .get(url.clone())
        .header("Range", format!("bytes={from}-{to}"))
        .send()
        .await?
        .error_for_status()?;

    let mut reader = Parser::new(resp.bytes_stream());

    let mut buf = [0u8; 4];
    let mut byte_offset = 0;

    while let Some(value) = reader.next().await? {
        buf[0] = value;
        buf.rotate_left(1);

        if buf == *b"PK\x05\x06" {
            if let MaybeEocd32::Eocd32(value) =
                read_eocd32(&mut reader, from + byte_offset, filesize).await?
            {
                return Ok(Some(value.into()));
            }
        } else if buf == *b"PK\x06\x06"
            && let Some(value) = read_eocd64(&mut reader, from + byte_offset).await?
        {
            return Ok(Some(value.into()));
        }

        byte_offset += 1;
    }

    Ok(None)
}

/// Read a EOCD32 record.
/// The given reader should return bytes right after the magic number `PK\x05\x06`.
async fn read_eocd32<S: ParserStream>(
    r: &mut Parser<S>,
    offset: usize,
    filesize: usize,
) -> Result<MaybeEocd32> {
    let this_disk_number = r.take_u16().await?;
    if this_disk_number > 256 && this_disk_number != 0xff {
        return Ok(MaybeEocd32::FalsePositive);
    }
    let cd_disk = r.take_u16().await?;
    if cd_disk > 256 && cd_disk != 0xff {
        return Ok(MaybeEocd32::FalsePositive);
    }
    let cd_records_on_disk = r.take_u16().await?;
    let cd_records_total = r.take_u16().await?;
    let cd_size = r.take_u32().await?;
    if cd_size as usize > filesize {
        return Ok(MaybeEocd32::FalsePositive);
    }
    let cd_offset = r.take_u32().await?;
    if cd_offset as usize > filesize {
        return Ok(MaybeEocd32::FalsePositive);
    }

    if this_disk_number == 0xff
        && cd_disk == 0xff
        && cd_records_on_disk == 0xff
        && cd_records_total == 0xff
        && cd_size == 0xffff
        && cd_offset == 0xffff
    {
        return Ok(MaybeEocd32::Zip64);
    }

    let comment_len = r.take_u16().await?;
    let _comment = r.skip_bytes(comment_len as usize).await?;

    Ok(MaybeEocd32::Eocd32(Eocd32 {
        this_disk_number,
        cd_disk,
        cd_records_on_disk,
        cd_records_total,
        cd_size,
        cd_offset,
        offset,
    }))
}

enum MaybeEocd32 {
    FalsePositive,
    Zip64,
    Eocd32(Eocd32),
}

/// Read a EOCD64 or None if it is a false positive.
/// The given reader should return bytes right after the magic number `PK\x06\x06`.
async fn read_eocd64<S: ParserStream>(r: &mut Parser<S>, offset: usize) -> Result<Option<Eocd64>> {
    let _size = r.take_u64().await?;
    let version_made_by = r.take_u16().await?;
    let version_to_extract = r.take_u16().await?;
    // The version is stored in the last 8 bits of the field,
    // if the version is larger than 63 it's likely a false positive.
    if (version_made_by & 0xff) > 63 || (version_to_extract & 0xff) > 63 {
        return Ok(None);
    }
    let this_disk_number = r.take_u32().await?;
    let cd_disk = r.take_u32().await?;
    let cd_records_on_disk = r.take_u64().await?;
    let cd_records_total = r.take_u64().await?;
    let cd_size = r.take_u64().await?;
    let cd_offset = r.take_u64().await?;
    //let _comment = r.read_bytes

    Ok(Some(Eocd64 {
        this_disk_number,
        cd_disk,
        cd_records_on_disk,
        cd_records_total,
        cd_size,
        cd_offset,
        offset,
    }))
}

/// Make a HEAD request and retrive the Content-Length header.
///
/// # Errors
/// If the Content-Length is not present or malformed.
// TODO: Currently, ziponline does not check if a url supports HTTP range requests
//       because some servers don't properly set the Accept-Ranges header,
//       but you COULD attempt a 0-0 byte range request and see if it fails to check instead.
//       TL;DR check if a url supports http range requests by sending a 0-0 byte range request.
async fn request_content_length(client: &Client, url: &Url) -> Result<usize> {
    let resp = client.head(url.clone()).send().await?.error_for_status()?;

    let Some(value) = resp.headers().get("content-length") else {
        return Err(Error::ContentLengthMissing);
    };

    let value = match value.to_str() {
        Ok(value) => value,
        Err(e) => return Err(Error::ContentLengthInvalidAscii(e)),
    };

    match value.parse() {
        Ok(value) => Ok(value),
        Err(e) => Err(Error::ContentLengthParse(e)),
    }
}

/// Shortcut for `Result<T, ziponline::Error>`
pub type Result<T> = std::result::Result<T, Error>;

/// Errors for failed HTTP requests and invalid zip file formats.
#[derive(thiserror::Error, Debug)]
pub enum Error {
    /// A request failure.
    #[error(transparent)]
    Request(#[from] reqwest::Error),
    /// When the filesize is not provided, ziponline will create a HEAD request to retrieve the filesize.
    /// If the HEAD request does not contain a `content-length` header this error will be returned.
    #[error("missing content-length header")]
    ContentLengthMissing,
    /// The `content-length` header of the request is not valid ASCII.
    #[error("content-length is not valid ascii")]
    ContentLengthInvalidAscii(ToStrError),
    /// The `content-length` header of the request could not be parsed as a number.
    #[error("couldn't parse content-length as a number: {0}")]
    ContentLengthParse(ParseIntError),
    /// The zip file stream ended unexpectedly.
    #[error("unexpected eof")]
    UnexpectedEof,
    /// The end of central directory header could not be found, this may be a sign of corruption.
    #[error("eocd not found")]
    EocdNotFound,
    /// When trying to extract a file, it could not be found.
    #[error("cd: file not found")]
    CdFileNotFound,
    /// A file header is malformed or broken, this may be a sign of corruption.
    #[error("malformed file header")]
    MalformedFileHeader,
}
