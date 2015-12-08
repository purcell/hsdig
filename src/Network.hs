module Network where


import           Control.Exception              (bracket)
import           Data.Binary                    (decode, decodeOrFail, encode)
import qualified Data.ByteString.Lazy           as BL
import           Network.Socket                 hiding (recv, recvFrom, send,
                                                 sendTo)
import           Network.Socket.ByteString.Lazy
import           Types



dnsConnect :: HostName -> IO Socket
dnsConnect server = do
  addrinfos <- getAddrInfo Nothing (Just server) (Just "domain")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  connect sock (addrAddress serveraddr)
  return sock


doRequest :: [String] -> HostName -> QuestionType -> IO Message
doRequest hostquery server qtype = bracket (dnsConnect server) close runQuery
  where runQuery :: Socket -> IO Message
        runQuery sock = do
          let message = Message 1 0 [Question (nameFromString hostquery) qtype ICDefault] [] [] []
          sendAll sock (encode message)
          received <- recv sock 512
          print received
          return . decode $ received
          -- case decodeOrFail received of
          --   Left (rest, consumed, err) -> do print ("Error after " ++ show consumed ++ " bytes: " ++ err); print rest; error err
          --   Right (_, _, response) -> return response



test = doRequest ["www", "google", "com"] "8.8.8.8" MX


-- "\NUL\SOH\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\ETXwww\ACKgoogle\ETXcom\NUL\NUL\SI\NUL\SOH"
-- "\NUL\SOH\128\130\NUL\SOH\NUL\NUL\NUL\NUL\NUL\NUL\ETXwww\ACKgoogle\ETXcom\NUL\NUL\SI\NUL\SOH"
