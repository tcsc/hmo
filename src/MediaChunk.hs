module MediaChunk (
  MediaChunk,
  ChunkHandler,
  ChunkSource(..)
) where
  
data MediaChunk = Chunk 

type ChunkHandler = MediaChunk -> IO ()

data ChunkSource = ChunkSource {
  chunkSourceRegister :: ChunkHandler -> IO ()
}