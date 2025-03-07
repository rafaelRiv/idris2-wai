module Network.Wai.Internal

import Network.HTTP.Types
import Network.Socket

data RequestBodyLength = ChunkedBody | KnownLength Bits64

Show RequestBodyLength where
  show ChunkedBody = "ChunkedBody"
  show (KnownLength x) = "KnownLength " ++ show x 

||| Information on the request sent by the client. This abstracts away the
||| details of the underlying implementation.
record Request where
  ||| Request method such as GET.
  requestMethod : Method
  ||| HTTP version such as 1.1.
  httpVersion : HttpVersion
  ||| Extra path information sent by the client. The meaning varies slightly
  ||| depending on backend; in a standalone server setting, this is most likely
  ||| all information after the domain name. In a CGI application, this would be
  ||| the information following the path to the CGI executable itself.
  |||
  ||| Middlewares and routing tools should not modify this raw value, as it may
  ||| be used for such things as creating redirect destinations by applications.
  ||| Instead, if you are writing a middleware or routing framework, modify the
  ||| @pathInfo@ instead. This is the approach taken by systems like Yesod
  ||| subsites.
  |||
  ||| /Note/: At the time of writing this documentation, there is at least one
  ||| system (@Network.Wai.UrlMap@ from @wai-extra@) that does not follow the
  ||| above recommendation. Therefore, it is recommended that you test the
  ||| behavior of your application when using @rawPathInfo@ and any form of
  ||| library that might modify the @Request@.
  rawPathInfo : String
  ||| If no query string was specified, this should be empty. This value
  ||| /will/ include the leading question mark.
  ||| Do not modify this raw value - modify queryString instead.
  rawQueryString : String
  ||| A list of headers (a pair of key and value) in an HTTP request.
  requestHeaders : RequestHeaders
  ||| Was this request made over an SSL connection?
  |||
  ||| Note that this value will /not/ tell you if the client originally made
  ||| this request over SSL, but rather whether the current connection is SSL.
  ||| The distinction lies with reverse proxies. In many cases, the client will
  ||| connect to a load balancer over SSL, but connect to the WAI handler
  ||| without SSL. In such a case, 'isSecure' will be 'False', but from a user
  ||| perspective, there is a secure connection.
  isSecure : Bool
  ||| The client\'s host information.
  remoteHost : SocketAddress
  ||| Path info in individual pieces - the URL without a hostname/port and
  ||| without a query string, split on forward slashes.
  pathInfo : List String
  ||| Parsed query string information.
  queryString : Query
  ||| Get the next chunk of the body. Returns 'B.empty' when the
  ||| body is fully consumed.
  getRequestBodyChunk : IO String
  ||| | A location for arbitrary data to be shared by applications and middleware.
  vault : List String
  ||| | The size of the request body. In the case of a chunked request body,
  ||| this may be unknown.
  |||
  ||| Since 1.4.0
  requestBodyLength : RequestBodyLength
  ||| | The value of the Host header in a HTTP request.
  |||
  ||| Since 2.0.0
  requestHeaderHost : Maybe String
  ||| | The value of the Range header in a HTTP request.
  |||
  ||| Since 2.0.0
  requestHeaderRange : Maybe String
  ||| | The value of the Referer header in a HTTP request.
  |||
  ||| Since 3.2.0
  requestHeaderReferer : Maybe String
  ||| | The value of the Referer header in a HTTP request.
  |||
  ||| Since 3.2.0
  requestHeaderUserAgent : Maybe String

Show Request where
    show request = "Request { " ++ show fields ++ " }"
        where
            fields =
                [("requestMethod",show $ requestMethod request)
                ,("httpVersion",show $ httpVersion request)
                ,("rawPathInfo",show $ rawPathInfo request)
                ,("rawQueryString",show $ rawQueryString request)
                ,("requestHeaders",show $ requestHeaders request)
                ,("isSecure",show $ isSecure request)
                ,("remoteHost",show $ remoteHost request)
                ,("pathInfo",show $ pathInfo request)
                ,("queryString",show $ queryString request)
                ,("requestBody","<IO String>")
                ,("vault","<Vault>")
                ,("requestBodyLength",show $ requestBodyLength request)
                ,("requestHeaderHost",show $ requestHeaderHost request)
                ,("requestHeaderRange",show $ requestHeaderRange request)
                ]

record FilePart where 
    filePartOffset    : Integer
    filePartByteCount : Integer
    filePartFileSize  : Integer

StreamingBody : Type
StreamingBody = (String -> IO ()) -> IO () -> IO ()

data Response
    = ResponseFile Status ResponseHeaders String (Maybe FilePart)
     | ResponseBuilder Status ResponseHeaders String
     | ResponseStream Status ResponseHeaders StreamingBody
     | ResponseRaw (IO String -> (String -> IO ()) -> IO ()) Response

ResponseReceived : Type 
