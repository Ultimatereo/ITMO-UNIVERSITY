{-# LANGUAGE DerivingVia #-}

module HW5.Action
  ( HiPermission (..)         -- Data type representing different permissions
  , PermissionException (..)   -- Exception type for permission-related errors
  , HIO (..)                   -- Monad transformer for IO actions with permissions
  ) where

import Codec.Binary.UTF8.String
import Control.Exception
import Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import Data.Set
import qualified Data.Text as Text
import Data.Time.Clock
import HW5.Base
import System.Directory
import System.Random

-- | Data type representing different permissions for IO actions
data HiPermission = AllowRead    -- Permission to read
                  | AllowWrite   -- Permission to write
                  | AllowTime    -- Permission to access time
                  deriving (Show, Eq, Ord)

-- | Exception type for permission-related errors
newtype PermissionException = PermissionRequired HiPermission deriving (Show)

instance Exception PermissionException

-- | Monad transformer for IO actions with permissions
newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad) via (ReaderT (Set HiPermission) IO)

-- | Typeclass instance for HIO monad
instance HiMonad HIO where
  -- Action to read from a file or directory
  runAction (HiActionRead path) = HIO
      { runHIO = \permissions ->
              if AllowRead `elem` permissions   -- Check if read permission is granted
              then do
                isDirectory <- doesDirectoryExist path
                if isDirectory
                then do
                  contents <- listDirectory path
                  pure $ HiValueList $ Seq.fromList $ Prelude.map (HiValueString . Text.pack) contents
                else do
                  fileContent <- BS.readFile path
                  let str = decode $ BS.unpack fileContent
                  if isUTF8Encoded str
                  then pure $ HiValueString $ Text.pack str
                  else pure $ HiValueBytes fileContent
              else throwIO $ PermissionRequired AllowRead   -- Throw exception if read permission is not granted
      }
  -- Action to write to a file
  runAction (HiActionWrite path bytes) = HIO
        { runHIO = \permissions ->
                if AllowWrite `elem` permissions   -- Check if write permission is granted
                then do
                  BS.writeFile path bytes
                  pure HiValueNull
                else throwIO $ PermissionRequired AllowWrite   -- Throw exception if write permission is not granted
        }
  -- Action to create a new directory
  runAction (HiActionMkDir path) = HIO
          { runHIO = \permissions ->
                if AllowWrite `elem` permissions   -- Check if write permission is granted
                then do
                  createDirectory path
                  pure HiValueNull
                else throwIO $ PermissionRequired AllowWrite   -- Throw exception if write permission is not granted
          }
  -- Action to change the working directory
  runAction (HiActionChDir path) = HIO
        { runHIO = \permissions ->
                if AllowRead `elem` permissions   -- Check if read permission is granted
                then do
                  setCurrentDirectory path
                  pure HiValueNull
                else throwIO $ PermissionRequired AllowRead   -- Throw exception if read permission is not granted
        }
  -- Action to get the current working directory
  runAction HiActionCwd = HIO
        { runHIO = \permissions ->
                if AllowRead `elem` permissions   -- Check if read permission is granted
                then HiValueString . Text.pack <$> getCurrentDirectory
                else throwIO $ PermissionRequired AllowRead   -- Throw exception if read permission is not granted
        }
  -- Action to get the current time
  runAction HiActionNow = HIO
          { runHIO = \permissions ->
                if AllowTime `elem` permissions   -- Check if time permission is granted
                then HiValueTime <$> getCurrentTime
                else throwIO $ PermissionRequired AllowTime   -- Throw exception if time permission is not granted
          }
  -- Action to get a random integral number within a range
  runAction (HiActionRand a b) = HIO
          { runHIO = \_ -> do
                n <- randomRIO (a, b)
                pure $ HiValueNumber $ toRational n
          }
  -- Action to print a string to stdout
  runAction (HiActionEcho text) = HIO
          { runHIO = \permissions ->
                if AllowWrite `elem` permissions   -- Check if write permission is granted
                then do
                  putStrLn $ Text.unpack text
                  pure HiValueNull
                else throwIO $ PermissionRequired AllowWrite   -- Throw exception if write permission is not granted
          }
