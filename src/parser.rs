use std::mem::{self, MaybeUninit};

use super::{Error, Result};
use bytes::Bytes;
use futures::{TryStream, TryStreamExt};

pub type BoxParserStream = Box<dyn ParserStream>;

/// Parse a `Stream` of `Bytes`.
/// Can be thought of as an iterator that returns `u8`,
/// with extra capability to return slices and little-endian numbers.
pub struct Parser<S: ParserStream> {
    stream: S,
    bytes: Bytes,
    is_eof: bool,
}

impl<S: ParserStream> Parser<S> {
    pub fn new(stream: S) -> Self {
        Self {
            stream,
            bytes: Bytes::new(),
            is_eof: false,
        }
    }
}

impl<S: ParserStream> Parser<S> {
    pub fn into_box(self) -> Parser<BoxParserStream>
    where
        S: 'static,
    {
        Parser {
            stream: Box::new(self.stream),
            bytes: self.bytes,
            is_eof: self.is_eof,
        }
    }

    /// Skip an amount of bytes in the stream.
    pub async fn skip_bytes(&mut self, length: usize) -> Result<()> {
        self.take_bytes(length).await.map(|_| ())
    }

    /// Take a list of bytes from the stream.
    pub async fn take_bytes(&mut self, length: usize) -> Result<Bytes> {
        self.request_atleast(length).await?;
        if self.bytes.len() < length {
            return Err(Error::UnexpectedEof);
        }
        Ok(self.bytes.split_to(length))
    }

    /// Take a little-endian u16 from the stream.
    pub async fn take_u16(&mut self) -> Result<u16> {
        Ok(u16::from_le_bytes(self.take_fixed_slice().await?))
    }

    /// Take a little-endian u32 from the stream.
    pub async fn take_u32(&mut self) -> Result<u32> {
        Ok(u32::from_le_bytes(self.take_fixed_slice().await?))
    }

    /// Take a little-endian u64 from the stream.
    pub async fn take_u64(&mut self) -> Result<u64> {
        Ok(u64::from_le_bytes(self.take_fixed_slice().await?))
    }

    /// Take a fixed slice of bytes from the stream.
    pub async fn take_fixed_slice<const N: usize>(&mut self) -> Result<[u8; N]> {
        self.request_atleast(N).await?;
        if self.bytes.len() < N {
            return Err(Error::UnexpectedEof);
        }

        let bytes = self.bytes.split_to(N);
        let mut slice: MaybeUninit<[u8; N]> = MaybeUninit::uninit();
        debug_assert_eq!(bytes.len(), N);

        for i in 0..N {
            unsafe { slice.assume_init_mut()[i] = bytes[i] }
        }

        Ok(unsafe { slice.assume_init() })
    }

    /// Get the next byte in the stream or None if it is finished.
    pub async fn next(&mut self) -> Result<Option<u8>> {
        self.request_atleast(1).await?;
        if self.bytes.is_empty() {
            return Ok(None);
        }
        Ok(Some(self.bytes.split_to(1)[0]))
    }

    /// If there isn't enough bytes, ask for more.
    pub async fn request_atleast(&mut self, atleast: usize) -> Result<()> {
        if self.bytes.len() < atleast && !self.is_eof {
            match self.stream.try_next().await? {
                Some(next) => {
                    self.bytes = [mem::take(&mut self.bytes), next].concat().into();
                }
                None => self.is_eof = true,
            }
        }
        Ok(())
    }
}

type ParserStreamItem = std::result::Result<Bytes, reqwest::Error>;
pub trait ParserStream:
    TryStream<Item = ParserStreamItem, Ok = Bytes, Error = reqwest::Error> + Unpin
{
}
impl<T: TryStream<Item = ParserStreamItem, Ok = Bytes, Error = reqwest::Error> + Unpin> ParserStream
    for T
{
}
