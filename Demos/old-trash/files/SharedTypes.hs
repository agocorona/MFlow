{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Data types shared between client and server.

module SharedTypes where

import Language.Fay.FFI
import Language.Fay.Prelude

-- | A phantom type which ensures the connection between the command
-- and the return value.
data Returns a = Returns
  deriving (Read,Data,Typeable,Show)

-- | The command list.
data Command
  = CheckModule String (Returns CheckResult)
  | GetModule String (Returns ModuleLoad)
  | LibraryModules (Returns ModuleList)
  | GlobalModules (Returns ModuleList)
  | InternalModules (Returns ModuleList)
  | ProjectModules (Returns ModuleList)
  | CompileModule String (Returns CompileResult)
  | CleanModuleName String (Returns ModuleNameCheck)
  | CreateModule String (Returns ModuleNameCheck)
  deriving (Read,Data,Typeable,Show)
instance Foreign Command

-- | A check result.
data CheckResult
  = CheckOk String
  | CheckError [Msg] String
  deriving (Read,Data,Typeable,Show)
instance Foreign CheckResult
instance Record CheckResult

-- | A check result.
data ModuleNameCheck
  = CleanModule String
  | InvalidModule String
  deriving (Read,Data,Typeable,Show)
instance Foreign ModuleNameCheck
instance Record ModuleNameCheck

-- | A check result.
data ModuleLoad
  = NoModule String
  | LoadedModule String
  deriving (Read,Data,Typeable,Show)
instance Foreign ModuleLoad
instance Record ModuleLoad

-- | A compile result.
data CompileResult
  = CompileOk String
  | CompileError String
  deriving (Read,Data,Typeable,Show)
instance Foreign CompileResult
instance Record CompileResult

-- | A msg.
data Msg
  = Msg MsgType Double String
  deriving (Read,Data,Typeable,Show)
instance Foreign Msg
instance Record Msg

-- | Message type.
data MsgType = MsgWarning | MsgError
  deriving (Read,Data,Typeable,Show,Eq)
instance Foreign MsgType

-- | Some text.
data Text = Text String
  deriving (Read,Data,Typeable,Show,Eq)
instance Foreign Text
instance Record Text

-- | A list of modules.
data ModuleList = ModuleList [String]
  deriving (Read,Data,Typeable,Show,Eq)
instance Foreign ModuleList
instance Record ModuleList

-- | A record type.
class Record a

