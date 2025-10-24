# ziponline

Extract files from a zip asynchronously without downloading the whole zip, using HTTP range requests.

# Limitations

Currently only supports DEFLATE decompression and only supports EOCD headers that are less than 256 bytes in size.

# Examples

Extract a single file
```rust
let mut reader = ziponline::extract_file(
    &client,    // the reqwest::Client
    &url,       // the url of the zip file
    None,       // the filesize or None if it is unknown
    "file.txt", // the name of the file to extract
).await?;

// `reader` is an io::Read object.
io::copy(&mut reader, ...);
```

Extract multiple files
```rust
let mut zip_file = LazyZipFile::new(
    client,   // the reqwest::Client
    url,      // the url of the zip file
    filesize, // the filesize or None if it is unknown
).await?;

let abc_reader = zip_file.extract_file("abc.txt").await?;
let def_reader = zip_file.extract_file("def.txt").await?;
let ghi_reader = zip_file.extract_file("ghi.txt").await?;
```