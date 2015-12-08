{-# LANGUAGE LambdaCase #-}
module Types where

import           Control.Monad         (forM_, replicateM)
import           Data.Binary
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Data.Maybe            (fromJust)
import           Data.Tuple            (swap)


data Message = Message { messageId         :: Word16
                       , messageFlags      :: Word16
                       , messageQuestion   :: [Question]
                       , messageAnswer     :: [Resource]
                       , messageAuthority  :: [Resource]
                       , messageAdditional :: [Resource]
                       } deriving (Show)

data Resource = Resource { resourceName  :: Name
                         , resourceType  :: Word16
                         , resourceClass :: InternetClass
                         , resourceTTL   :: Word32
                         , resourceData  :: ByteString }
                deriving (Show)

data Question = Question { questionName  :: Name
                         , questionType  :: QuestionType
                         , questionClass :: InternetClass }
                deriving (Show)

data Name = Name [ByteString] deriving (Show)
data QuestionType = A | NS | CNAME | PTR | MX | SRV | IXFR | AXFR | ALL deriving (Eq, Show)
data InternetClass = ICDefault deriving (Show)


-- Binary instances

instance Binary Message where
  put m = do
    put (messageId m)
    put (messageFlags m)
    put (len (messageQuestion m))
    put (len (messageAnswer m))
    put (len (messageAuthority m))
    put (len (messageAdditional m))
    mapM_ put (messageQuestion m)
    mapM_ put (messageAnswer m)
    mapM_ put (messageAuthority m)
    mapM_ put (messageAdditional m)
    where
      len x = fromIntegral (length x) :: Word16
  get = do
    mid <- get
    flags <- get
    nquestion <- get :: Get Word16
    nanswer <- get :: Get Word16
    nauthority <- get :: Get Word16
    nadditional <- get :: Get Word16
    questions <- replicateM (fromIntegral nquestion) get
    answers <- replicateM (fromIntegral nanswer) get
    authority <- replicateM (fromIntegral nauthority) get
    additional <- replicateM (fromIntegral nadditional) get
    return $ Message mid flags questions answers authority additional

instance Binary Resource where
  put r = do
    put (resourceName r)
    put (resourceType r)
    put (resourceClass r)
    put (resourceTTL r)
    put ((fromIntegral $ B.length (resourceData r))::Word16)
    putBytestring (resourceData r)
  get = Resource <$> get <*> get <*> get <*> get <*> (getWord8 >>= getBytestring)

getBytestring :: Integral a => a -> Get ByteString
getBytestring n = B.pack <$> replicateM (fromIntegral n) getWord8

putBytestring :: ByteString -> Put
putBytestring s = forM_ (B.unpack s) putWord8

instance Binary Question where
  put q = put (questionName q) >> put (questionType q) >> put (questionClass q)
  get = Question <$> get <*> get <*> get

instance Binary Name where
  put (Name parts) = do
    forM_ parts (\part -> putWord8 (fromIntegral $ B.length part) >> putBytestring part)
    put (0x00::Word8)
  get = Name <$> getparts
    where
      getparts :: Get [ByteString]
      getparts = do
        len <- getWord8
        if len > 0
          then do
               cur <- getBytestring len
               (cur:) <$> getparts
          else return []

instance Binary InternetClass where
  put ICDefault = put (0x0001 :: Word16)
  get = return ICDefault

questionTypeFlags :: [(QuestionType, Word16)]
questionTypeFlags = [
  (A, 0x01), (NS, 0x02), (CNAME, 0x05), (PTR, 0x0C), (MX, 0x0F),
  (SRV, 0x21), (IXFR, 0xFB), (AXFR, 0xFC), (ALL, 0xFF)
  ]

instance Binary QuestionType where
  put qt = put $ fromJust (lookup qt questionTypeFlags)
  get = (fromJust . flip lookup (map swap questionTypeFlags)) <$> get


----------

nameFromString :: [String] -> Name
nameFromString parts = Name (map B8.pack parts)

exampleQuestion = Question (nameFromString ["www", "looktothestars", "org"]) MX ICDefault
exampleMessage = Message 1 0 [exampleQuestion] [] [] []
