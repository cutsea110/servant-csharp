module CS.JsonDotNet.Internal.Types where

import Data.Aeson
import Data.Text

data FieldType = FInteger
               | FNumber
               | FString
               | FBool
               | FDay
               | FUTCTime
               | FEnum Text [Value]
               | FObject Text [(Text, FieldType)]
               | FList FieldType
               | FNullable FieldType

               | FRefObject Text
               | FRefEnum Text
               | FRefPrim Text FieldType
                 deriving Show

isFEnum :: FieldType -> Bool
isFEnum (FEnum _ _) = True
isFEnum _ = False

isFPrim :: FieldType -> Bool
isFPrim FString = True
isFPrim FInteger = True
isFPrim FNumber = True
isFPrim FBool = True
isFPrim FDay = True
isFPrim FUTCTime = True
isFPrim _ = False

isFObj :: FieldType -> Bool
isFObj (FObject _ _) = True
isFObj _ = False

data CCate = CVal | CRef | CSt deriving Show

nullable :: FieldType -> CCate
nullable FInteger = CVal
nullable FNumber = CVal
nullable FString = CRef
nullable FBool = CVal
nullable FDay = CVal
nullable FUTCTime = CVal
nullable (FEnum _ _) = CSt
nullable (FObject _ _) = CSt
nullable (FList _) = CRef
nullable (FNullable _) = CRef
nullable (FRefObject _) = CVal
nullable (FRefEnum _) = CVal
nullable (FRefPrim _ t) = nullable t

data ConverterType = NoConv
                   | DayConv
                   | EnumConv
                   | ItemConv ConverterType
                     deriving Show
