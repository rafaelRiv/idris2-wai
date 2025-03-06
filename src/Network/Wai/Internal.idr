module Network.Wai.Internal

import Network.HTTP.Types
-- import           Data.ByteString.Builder      (Builder)
-- import qualified Data.ByteString              as B hiding (pack)
-- import           Data.Text                    (Text)
-- import           Data.Typeable                (Typeable)
-- import           Data.Vault.Lazy              (Vault)
-- import           Data.Word                    (Word64)
-- import qualified Network.HTTP.Types           as H
-- import           Network.Socket               (SockAddr)
-- import           Data.List                    (intercalate)

||| Information on the request sent by the client. This abstracts away the
||| details of the underlying implementation.

record Request where
  requestMethod : Method
{-  httpVersion : H.HttpVersion
  rawPathInfo : B.ByteString
  rawQueryString : B.ByteString
  requestHeaders : H.RequestHeaders
  isSecure : Bool
  remoteHost : SockAddr
  pathInfo : [Text]
  queryString : H.Query
  getRequestBodyChunk : IO B.ByteString
  vault : Vault
  requestBodyLength : RequestBodyLength
  requestHeaderHost : Maybe B.ByteString
  requestHeaderRange : Maybe B.ByteString
  requestHeaderReferer : Maybe B.ByteString
  requestHeaderUserAgent : Maybe B.ByteString -}

{-
implementation Show Request where
    show Request{..} = "Request {" ++ intercalate ", " [a ++ " = " ++ b | (a,b) <- fields] ++ "}"
        where
            fields =
                [("requestMethod",show requestMethod)
                ,("httpVersion",show httpVersion)
                ,("rawPathInfo",show rawPathInfo)
                ,("rawQueryString",show rawQueryString)
                ,("requestHeaders",show requestHeaders)
                ,("isSecure",show isSecure)
                ,("remoteHost",show remoteHost)
                ,("pathInfo",show pathInfo)
                ,("queryString",show queryString)
                ,("requestBody","<IO ByteString>")
                ,("vault","<Vault>")
                ,("requestBodyLength",show requestBodyLength)
                ,("requestHeaderHost",show requestHeaderHost)
                ,("requestHeaderRange",show requestHeaderRange)
                ] -}


-- data Response
--     = ResponseFile H.Status H.ResponseHeaders FilePath (Maybe FilePart)
--     | ResponseBuilder H.Status H.ResponseHeaders Builder
--     | ResponseStream H.Status H.ResponseHeaders StreamingBody
--     | ResponseRaw (IO B.ByteString -> (B.ByteString -> IO ()) -> IO ()) Response
--   deriving Typeable

-- -- | Represents a streaming HTTP response body. It's a function of two
-- -- parameters; the first parameter provides a means of sending another chunk of
-- -- data, and the second parameter provides a means of flushing the data to the
-- -- client.
-- --
-- -- Since 3.0.0
-- type StreamingBody = (Builder -> IO ()) -> IO () -> IO ()

-- -- | The size of the request body. In the case of chunked bodies, the size will
-- -- not be known.
-- --
-- -- Since 1.4.0
-- data RequestBodyLength = ChunkedBody | KnownLength Word64 deriving Show

-- -- | Information on which part to be sent.
-- --   Sophisticated application handles Range (and If-Range) then
-- --   create 'FilePart'.
-- data FilePart = FilePart
--     { filePartOffset    :: Integer
--     , filePartByteCount :: Integer
--     , filePartFileSize  :: Integer
--     } deriving Show

-- -- | A special datatype to indicate that the WAI handler has received the
-- -- response. This is to avoid the need for Rank2Types in the definition of
-- -- Application.
-- --
-- -- It is /highly/ advised that only WAI handlers import and use the data
-- -- constructor for this data type.
-- --
-- -- Since 3.0.0
-- data ResponseReceived = ResponseReceived
--     deriving Typeable
