#![allow(
    dead_code,
    reason = "unused fields still need to be parsed so may as well store them"
)]

/// End of central directory record.
#[derive(Debug)]
pub struct Eocd {
    pub this_disk_number: u32,
    pub cd_disk: u32,
    pub cd_records_on_disk: u64,
    pub cd_records_total: u64,
    pub cd_size: u64,
    pub cd_offset: u64,
    pub offset: usize,
    pub is_zip64: bool,
}

impl From<Eocd32> for Eocd {
    fn from(value: Eocd32) -> Self {
        Self {
            this_disk_number: value.this_disk_number as u32,
            cd_disk: value.cd_disk as u32,
            cd_records_on_disk: value.cd_records_on_disk as u64,
            cd_records_total: value.cd_records_total as u64,
            cd_size: value.cd_size as u64,
            cd_offset: value.cd_offset as u64,
            offset: value.offset,
            is_zip64: false,
        }
    }
}

impl From<Eocd64> for Eocd {
    fn from(value: Eocd64) -> Self {
        Self {
            this_disk_number: value.this_disk_number,
            cd_disk: value.cd_disk,
            cd_records_on_disk: value.cd_records_on_disk,
            cd_records_total: value.cd_records_total,
            cd_size: value.cd_size,
            cd_offset: value.cd_offset,
            offset: value.offset,
            is_zip64: true,
        }
    }
}

/// End of central directory record for classic 32-bit zips.
#[derive(Debug)]
pub struct Eocd32 {
    pub this_disk_number: u16,
    pub cd_disk: u16,
    pub cd_records_on_disk: u16,
    pub cd_records_total: u16,
    pub cd_size: u32,
    pub cd_offset: u32,
    pub offset: usize,
}

/// End of central directory record for Zip64.
#[derive(Debug)]
pub struct Eocd64 {
    pub this_disk_number: u32,
    pub cd_disk: u32,
    pub cd_records_on_disk: u64,
    pub cd_records_total: u64,
    pub cd_size: u64,
    pub cd_offset: u64,
    pub offset: usize,
}

/// Central directory file header.
pub struct Cdfh {
    pub version_made_by: u16,
    pub version_to_extract: u16,
    pub general_purpose_flags: u16,
    pub compression_method: CompressionMethod,
    pub last_modification_time: u16,
    pub last_modification_date: u16,
    pub crc32: u32,
    pub compressed_size: u32,
    pub uncompressed_size: u32,
    pub extra_field_length: u16,
    pub file_comment_length: u16,
    pub disk_number: u16,
    pub internal_attrs: u16,
    pub external_attrs: u32,
    pub file_header_offset: u32,
    pub filename: String,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CompressionMethod {
    Stored,
    Deflated,
    Deflate64,
    Bzip2,
    Lzma,
    Zstd,
    Xz,
    Aes,
}

impl CompressionMethod {
    pub fn from_id(id: u16) -> Option<CompressionMethod> {
        Some(match id {
            0 => Self::Stored,
            8 => Self::Deflated,
            9 => Self::Deflate64,
            12 => Self::Bzip2,
            14 => Self::Lzma,
            93 => Self::Zstd,
            95 => Self::Xz,
            99 => Self::Aes,
            _ => return None,
        })
    }
}
