{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CS.JsonDotNet ( genCsForAPI

                     , classCsForAPI
                     , classCsForAPIWith
                     , apiCsForAPI
                     , apiCsForAPIWith
                     , enumCsForAPI
                     , enumCsForAPIWith
                     , converterCsForAPI
                     , converterCsForAPIWith
                     , assemblyInfoCsForAPI
                     , assemblyInfoCsForAPIWith
                     , csprojForAPI
                     , csprojForAPIWith

                     , GenerateCsConfig(..)
                     , def
                     ) where

import Prelude hiding (concat, lines, unlines)
import Control.Arrow
import Control.Lens hiding ((<.>))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 as BC (unpack)
import Data.Char (toUpper, toLower)
import Data.List (intercalate, concat)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text as T (Text, unpack, pack)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Time.Calendar (toGregorian)
import Data.UUID.Types (toString)
import Data.UUID.V4 as UUID (nextRandom)
import Language.Haskell.Exts
import Servant.Foreign
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))
import Text.Heredoc

import CS.Common (CSharp, getEndpoints)

data GenerateCsConfig
    = GenerateCsConfig { namespace :: String
                       , outdir :: String
                       , classCsName :: String
                       , apiCsName :: String
                       , enumCsName :: String
                       , converterCsName :: String
                       , classtemplate :: GenerateCsConfig -> IO String
                       , apitemplate ::  forall api.
                                         (HasForeign CSharp Text api,
                                          GenerateList Text (Foreign Text api))
                                         => GenerateCsConfig
                                     -> Proxy api
                                     -> IO String
                       , enumtemplate :: GenerateCsConfig -> IO String
                       , convertertemplate :: GenerateCsConfig -> IO String
                       , assemblyinfotemplate :: GenerateCsConfig -> IO String
                       , csprojtemplate :: GenerateCsConfig -> IO String
                       , guid :: Maybe String
                       , sources :: [FilePath]
                       }

def :: GenerateCsConfig
def = GenerateCsConfig { namespace = "ServantClientAPI"
                       , outdir = "gen"
                       , classCsName = "Classes.cs"
                       , apiCsName = "API.cs"
                       , enumCsName = "Enum.cs"
                       , converterCsName = "JsonConverter.cs"
                       , classtemplate = defClassTemplate
                       , apitemplate = defAPITemplate
                       , enumtemplate = defEnumTemplate
                       , convertertemplate = defConvTemplate
                       , assemblyinfotemplate = defAssemblyInfoTemplate
                       , csprojtemplate = defCsprojTemplate
                       , guid = Nothing
                       , sources = []
                       }

genCsForAPI :: (HasForeign CSharp Text api,
             GenerateList Text (Foreign Text api)) =>
            GenerateCsConfig -> Proxy api -> IO ()
genCsForAPI conf api = do
  guid' <- maybe (toString <$> UUID.nextRandom) return $ guid conf
  let conf' = conf { guid = Just guid' }
  createDirectoryIfMissing True $ outdir conf' </> namespace conf' </> "Properties"
  classCsForAPIWith conf' >>= writeFile (outdir conf' </> namespace conf' </> classCsName conf')
  apiCsForAPIWith conf' api >>= writeFile (outdir conf' </> namespace conf' </> apiCsName conf')
  enumCsForAPIWith conf' >>= writeFile (outdir conf' </> namespace conf' </> enumCsName conf')
  converterCsForAPIWith conf' >>= writeFile (outdir conf' </> namespace conf' </> converterCsName conf')
  assemblyInfoCsForAPIWith conf' >>= writeFile (outdir conf' </> namespace conf' </> "Properties" </> "AssemblyInfo.cs")
  csprojForAPIWith conf' >>= writeFile (outdir conf' </> namespace conf' </> namespace conf' <.> "csproj")

--------------------------------------------------------------------------
isDatatypeDecl :: Decl -> Bool
isDatatypeDecl (DataDecl _ DataType _ _ _ [qcon] _) = True
isDatatypeDecl _ = False

data FieldType = TInt
               | TString
               | TDay
               | TUTCTime
               | TEnum String
               | TGeneral String
               | TNewtype String FieldType
               | TList FieldType
               | TNullable FieldType

instance Show FieldType where
    show TInt = "int"
    show TString = "string"
    show TDay = "DateTime"
    show TUTCTime = "DateTime"
    show (TEnum s) = s
    show (TGeneral s) = s
    show (TNewtype s _) = s
    show (TList t) = "List<"<>show t<>">"
    show (TNullable TInt) = "int?"
    show (TNullable TString) = "string"
    show (TNullable TDay) = "DateTime?"
    show (TNullable TUTCTime) = "DateTime?"
    show (TNullable (TEnum t)) = show (TEnum t)<>"?"
    show (TNullable (TNewtype s TString)) = s
    show (TNullable (TNewtype s _)) = "Nullable<"<>s<>">"
    show (TNullable t) = "Nullable<"<>show t<>">"

showCSharpOriginalType :: FieldType -> String
showCSharpOriginalType TInt = "System.Int64"
showCSharpOriginalType TString = "System.String"
showCSharpOriginalType _ = error "don't support this type."

classTypes :: GenerateCsConfig -> IO [(String, [(String, FieldType)])]
classTypes conf = do
  enums <- fmap (map fst) $ enumTypes conf
  aliases <- usingAliases conf
  classTypesFromFiles enums aliases (sources conf)
    where
      classTypesFromFiles :: [String] -> [(String, FieldType)] -> [FilePath]
                          -> IO [(String, [(String, FieldType)])]
      classTypesFromFiles enums aliases hss
          = return . concat =<< mapM (classTypesFromFile enums aliases) hss

      classTypesFromFile :: [String] -> [(String, FieldType)] -> FilePath
                         -> IO [(String, [(String, FieldType)])]
      classTypesFromFile enums aliases hs = do
        ParseOk (Module _ _ _ _ _ _ decls) <- parseFile hs
        let xs = filter isDatatypeDecl decls
        return $ map toClass xs
            where
              toClass (DataDecl _ _ _ _ _ [qcon] _)
                  = toClass' qcon
              toClass' (QualConDecl _ _ _ (RecDecl (Ident name) fs))
                  = (name, map field fs)
              field ((Ident fname):[], ts)
                  = (fname, toType ts)
              toType :: Type -> FieldType
              toType (TyCon (UnQual (Ident t)))
                  = case t of
                      "String" -> TString
                      "Text" -> TString
                      "Int" -> TInt
                      "Integer" -> TInt
                      "Day" -> TDay
                      "UTCTime" -> TUTCTime
                      _ -> if t `elem` enums
                           then TEnum t
                           else maybe (TGeneral t) (TNewtype t)
                                    $ lookup t aliases
              toType (TyApp (TyCon (UnQual (Ident "Maybe"))) t)
                  = case toType t of
                      TList t -> TList t
                      t -> TNullable t
              toType (TyList t) = TList (toType t)
              toType _ = error "don't support this Type"

--------------------------------------------------------------------------
isEnumLikeDataDecl :: Decl -> Bool
isEnumLikeDataDecl (DataDecl _ DataType _ _ _ xs _)
    = all isEnumLikeConDecl xs
isEnumLikeDataDecl _ = False

isEnumLikeConDecl :: QualConDecl -> Bool
isEnumLikeConDecl (QualConDecl _ _ _ (ConDecl _ [])) = True
isEnumLikeConDecl _ = False

enumTypes :: GenerateCsConfig -> IO [(String, [String])]
enumTypes = enumTypesFromFiles . sources
    where
      enumTypesFromFiles :: [FilePath] -> IO [(String, [String])]
      enumTypesFromFiles hss
          = return . concat =<< mapM enumTypesFromFile hss

      enumTypesFromFile :: FilePath -> IO [(String, [String])]
      enumTypesFromFile hs = do
        ParseOk (Module _ _ _ _ _ _ decls) <- parseFile hs
        let xs = filter isEnumLikeDataDecl decls
        return $ map toTuple xs
            where
              conName :: QualConDecl -> String
              conName (QualConDecl _ _ _ (ConDecl (Ident name) [])) = name
              conName _ = error "invalid enum type"
              toTuple (DataDecl _ _ _ (Ident name) _ xs _)
                  = (name, map conName xs)

--------------------------------------------------------------------------
-- | TODO : more typeable
isNewtypeDecl :: Decl -> Bool
isNewtypeDecl (DataDecl _ NewType _ _ _ _ _) = True
isNewtypeDecl _ = False

isTypeDecl :: Decl -> Bool
isTypeDecl (TypeDecl _ _ _ _) = True
isTypeDecl _ = False

origType :: QualConDecl -> FieldType
origType (QualConDecl _ _ _ (RecDecl _ [(_, tycon)]))
    = origType' tycon

origType' :: Type -> FieldType
origType' (TyCon (UnQual (Ident t)))
    = case t of
        "String"  -> TString
        "Text"    -> TString
        "Int"     -> TInt
        "Integer" -> TInt
        t         -> error ("don't supported type. "<>t)

usingAliases :: GenerateCsConfig -> IO [(String, FieldType)]
usingAliases = usingAliasesFromFiles . sources
    where
      usingAliasesFromFiles :: [FilePath] -> IO [(String, FieldType)]
      usingAliasesFromFiles hss
          = return . concat =<< mapM usingAliasesFromFile hss

      usingAliasesFromFile :: FilePath -> IO [(String, FieldType)]
      usingAliasesFromFile hs = do
        ParseOk (Module _ _ _ _ _ _ decls) <- parseFile hs
        let xs = filter (\d -> isNewtypeDecl d || isTypeDecl d) decls
        return $ map toTuple xs
            where
              toTuple (DataDecl _ NewType _ (Ident name) _ [qcon] _)
                  = (name, origType qcon)
              toTuple (TypeDecl _ (Ident name) _ tycon)
                  = (name, origType' tycon)

--------------------------------------------------------------------------
retType :: Req Text -> String
retType = T.unpack . fromJust . view reqReturnType

uri :: Req Text -> String
uri req = T.unpack $ segmentsToText $ req^..reqUrl.path.traverse
    where
      segmentsToText :: [Segment f] -> Text
      segmentsToText = foldr segToText ""
      segToText :: Segment f -> Text -> Text
      segToText (Segment (Static s)) ss
          = "/" <> s^._PathSegment <> ss
      segToText (Segment (Cap s)) ss
          = "/{" <> prefix <> s^.argName._PathSegment <> "}" <> ss
      prefix = "_"

methodType :: Req Text -> String
methodType = capitalize . BC.unpack . view reqMethod
    where
      capitalize :: String -> String
      capitalize (c:cs) = toUpper c:map toLower cs

methodName :: Req Text -> String
methodName  = T.unpack . view (reqFuncName.camelCaseL)

paramDecl :: Req Text -> String
paramDecl = intercalate ", " . map help . paramInfos True
    where
      help :: (String, String) -> String
      help (t, n) = t<>" "<>(prefix<>n)
      prefix = "_"

paramArg :: Req Text -> String
paramArg = intercalate ", " . map help . paramInfos False
    where
      help :: (String, String) -> String
      help (_, n) = prefix<>n
      prefix = "_"

paramInfos :: Bool -> Req Text -> [(String, String)]
paramInfos b req = foldr (<>) mempty
                   $ map ($ req) [ captures
                                 , rqBody
                                 , queryparams'
                                 ]
    where
      queryparams' = map (help b) . queryparams
          where
            help True  = convToNullable *** (<>" = null")
            help False = convToNullable *** id
            -- TODO : more typeable
            convToNullable "int" = "int?"
            convToNullable "string" = "string"
            convToNullable "DateTime" = "DateTime?"
            convToNullable t = "Nullable<"<>t<>">"

queryparams :: Req Text -> [(String, String)]
queryparams req = map ((T.unpack . view argType
                       &&&
                        T.unpack . unPathSegment . view argName)
                      . view queryArgName)
                  $ req^..reqUrl.queryStr.traverse

captures :: Req Text -> [(String, String)]
captures req = map ((T.unpack . view argType &&& T.unpack . view argPath)
                    . captureArg)
               . filter isCapture
               $ req^.reqUrl.path

rqBody :: Req Text -> [(String, String)]
rqBody req = maybe [] (pure . (T.unpack &&& const jsonReqBodyName))
             $ req^.reqBody
    where
      jsonReqBodyName = "obj"

requestBodyExists :: Req Text -> Bool
requestBodyExists = not . null . rqBody

classCsForAPI :: IO String
classCsForAPI = classCsForAPIWith def

classCsForAPIWith :: GenerateCsConfig -> IO String
classCsForAPIWith conf = (classtemplate conf) conf

apiCsForAPI :: (HasForeign CSharp Text api,
             GenerateList Text (Foreign Text api)) =>
            Proxy api -> IO String
apiCsForAPI = apiCsForAPIWith def

apiCsForAPIWith :: (HasForeign CSharp Text api,
                 GenerateList Text (Foreign Text api)) =>
                GenerateCsConfig -> Proxy api -> IO String
apiCsForAPIWith conf api = (apitemplate conf) conf api

enumCsForAPI :: IO String
enumCsForAPI = enumCsForAPIWith def

enumCsForAPIWith :: GenerateCsConfig -> IO String
enumCsForAPIWith conf = (enumtemplate conf) conf

converterCsForAPI :: IO String
converterCsForAPI = converterCsForAPIWith def

converterCsForAPIWith :: GenerateCsConfig -> IO String
converterCsForAPIWith conf = (convertertemplate conf) conf

defClassTemplate :: GenerateCsConfig -> IO String
defClassTemplate conf = do
  uas <- usingAliases conf
  classes <- classTypes conf
  return [heredoc|/* generated by servant-csharp */
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;
using System;
using System.Collections.Generic;

#region type alias
$forall (n, t) <- uas
  using ${n} = ${showCSharpOriginalType t};
#endregion

namespace ${namespace conf}
{
    $forall (name, fields) <- classes
      #region ${name}
      [JsonObject("${name}")]
      public class ${name}
      {
          $forall (fname, ftype) <- fields
            $case ftype
              $of TDay
                [JsonProperty(PropertyName = "${fname}")]
                [JsonConverter(typeof(DayConverter))]
              $of TNullable TDay
                [JsonProperty(PropertyName = "${fname}")]
                [JsonConverter(typeof(DayConverter))]
              $of TEnum _
                [JsonProperty(PropertyName = "${fname}")]
                [JsonConverter(typeof(StringEnumConverter))]
              $of TNullable (TEnum _)
                [JsonProperty(PropertyName = "${fname}")]
                [JsonConverter(typeof(StringEnumConverter))]
              $of TList (TEnum _)
                [JsonProperty(PropertyName = "${fname}", ItemConverterType = typeof(StringEnumConverter))]
              $of _
                [JsonProperty(PropertyName = "${fname}")]
            public ${show ftype} ${fname} { get; set; }
      }
      #endregion
}
|]

defAPITemplate :: (HasForeign CSharp Text api,
                GenerateList Text (Foreign Text api)) =>
               GenerateCsConfig -> Proxy api -> IO String
defAPITemplate conf api = do
  uas <- usingAliases conf
  return [heredoc|/* generated by servant-csharp */
using Newtonsoft.Json;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;

#region type alias
$forall (n, t) <- uas
  using ${n} = ${showCSharpOriginalType t};
#endregion

namespace ${namespace conf}
{
    class ServantClient : HttpClient
    {
        public ServantClient()
        {
            this.DefaultRequestHeaders.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
        }
    }

    public class API
    {
        #region fields
        private string server;
        #endregion

        #region properties
        #endregion

        #region Constructor
        public API(string _server)
        {
            this.server = _server;
        }
        #endregion

        #region APIs
        $forall ep <- getEndpoints api
          $if retType ep /= "void"
            public async Task<${retType ep}> ${methodName ep}Async(${paramDecl ep})
          $else
            public async Task ${methodName ep}Async(${paramDecl ep})
          {
              var client = new ServantClient();
              var queryparams = new List<string> {
                  $forall (_, qp) <- queryparams ep
                    _${qp}.HasValue ? $"_${qp}={_${qp}.Value}" : null,
              }.Where(e => !string.IsNullOrEmpty(e));
              var qp= queryparams.Count() > 0 ? $"?{string.Join("&", queryparams)}" : "";
              $if requestBodyExists ep
                #if DEBUG
                var jsonObj = JsonConvert.SerializeObject(_obj, Formatting.Indented);
                #else
                var jsonObj = JsonConvert.SerializeObject(_obj);
                #endif
              $if requestBodyExists ep
                var res = await client.${methodType ep}Async($"{server}${uri ep}{qp}", new StringContent(jsonObj, Encoding.UTF8, "application/json"));
              $else
                var res = await client.${methodType ep}Async($"{server}${uri ep}{qp}");
              Debug.WriteLine($">>> {res.RequestMessage}");
              $if requestBodyExists ep
                Debug.WriteLine($"-----");
                Debug.WriteLine(jsonObj);
                Debug.WriteLine($"-----");
              Debug.WriteLine($"<<< {(int)res.StatusCode} {res.ReasonPhrase}");
              var content = await res.Content.ReadAsStringAsync();
              Debug.WriteLine($"<<< {content}");
              $if retType ep /= "void"
                return JsonConvert.DeserializeObject<${retType ep}>(content);
              $else
                JsonConvert.DeserializeObject(content);
         }
          public ${retType ep} ${methodName ep}(${paramDecl ep})
          {
              $if retType ep /= "void"
                Task<${retType ep}> t = ${methodName ep}Async(${paramArg ep});
                return t.GetAwaiter().GetResult();
              $else
                Task t = ${methodName ep}Async(${paramArg ep});
                t.GetAwaiter().GetResult();
          }
        #endregion
    }
}
|]

defEnumTemplate conf = do
  es <- enumTypes conf
  return [heredoc|/* generated by servant-csharp */
namespace ${namespace conf}
{
    $forall (name, cs) <- es
      #region ${name}
      public enum ${name}
      {
          $forall c <- cs
            ${c},
      }
      #endregion
}
|]

defConvTemplate conf = do
  return [heredoc|/* generated by servant-csharp */
using Newtonsoft.Json;
using System;

namespace ${namespace conf}
{
    public class DayConverter : JsonConverter
    {
        public override bool CanConvert(Type objectType)
        {
            return objectType == typeof(DateTime);
        }

        public override object ReadJson(JsonReader reader, Type objectType, object existingValue, JsonSerializer serializer)
        {
            return DateTime.Parse((string)reader.Value);
        }

        public override void WriteJson(JsonWriter writer, object value, JsonSerializer serializer)
        {
            DateTime d = (DateTime)value;
            writer.WriteValue(d.ToString("yyyy-MM-dd"));
        }
    }
}
|]

--------------------------------------------------------------------------

assemblyInfoCsForAPI :: IO String
assemblyInfoCsForAPI = assemblyInfoCsForAPIWith def

assemblyInfoCsForAPIWith :: GenerateCsConfig -> IO String
assemblyInfoCsForAPIWith conf = (assemblyinfotemplate conf) conf

defAssemblyInfoTemplate :: GenerateCsConfig -> IO String
defAssemblyInfoTemplate conf = do
  (year, _, _) <- fmap (toGregorian . utctDay) getCurrentTime
  guid <- maybe (toString <$> UUID.nextRandom) return $ guid conf
  return [heredoc|
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[assembly: AssemblyTitle("${namespace conf}")]
[assembly: AssemblyDescription("")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("${namespace conf}")]
[assembly: AssemblyCopyright("Copyright ©  ${show year}")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]

[assembly: ComVisible(false)]

[assembly: Guid("${guid}")]

// [assembly: AssemblyVersion("1.0.*")]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyFileVersion("1.0.0.0")]
|]

--------------------------------------------------------------------------

csprojForAPI :: IO String
csprojForAPI = csprojForAPIWith def

csprojForAPIWith :: GenerateCsConfig -> IO String
csprojForAPIWith conf = (csprojtemplate conf) conf

defCsprojTemplate :: GenerateCsConfig -> IO String
defCsprojTemplate conf = do
  guid <- maybe ((map toUpper . toString) <$> UUID.nextRandom) return $ guid conf
  return [heredoc|<?xml version="1.0" encoding="utf-8"?>
<Project ToolsVersion="14.0" DefaultTargets="Build" xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
  <Import Project="$(MSBuildExtensionsPath)\$(MSBuildToolsVersion)\Microsoft.Common.props" Condition="Exists('$(MSBuildExtensionsPath)\$(MSBuildToolsVersion)\Microsoft.Common.props')" />
  <PropertyGroup>
    <Configuration Condition=" '$(Configuration)' == '' ">Debug</Configuration>
    <Platform Condition=" '$(Platform)' == '' ">AnyCPU</Platform>
    <ProjectGuid>{${guid}}</ProjectGuid>
    <OutputType>Library</OutputType>
    <AppDesignerFolder>Properties</AppDesignerFolder>
    <RootNamespace>${namespace conf}</RootNamespace>
    <AssemblyName>${namespace conf}</AssemblyName>
    <TargetFrameworkVersion>v4.5.2</TargetFrameworkVersion>
    <FileAlignment>512</FileAlignment>
  </PropertyGroup>
  <PropertyGroup Condition=" '$(Configuration)|$(Platform)' == 'Debug|AnyCPU' ">
    <DebugSymbols>true</DebugSymbols>
    <DebugType>full</DebugType>
    <Optimize>false</Optimize>
    <OutputPath>bin\Debug\</OutputPath>
    <DefineConstants>DEBUG;TRACE</DefineConstants>
    <ErrorReport>prompt</ErrorReport>
    <WarningLevel>4</WarningLevel>
  </PropertyGroup>
  <PropertyGroup Condition=" '$(Configuration)|$(Platform)' == 'Release|AnyCPU' ">
    <DebugType>pdbonly</DebugType>
    <Optimize>true</Optimize>
    <OutputPath>bin\Release\</OutputPath>
    <DefineConstants>TRACE</DefineConstants>
    <ErrorReport>prompt</ErrorReport>
    <WarningLevel>4</WarningLevel>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="Newtonsoft.Json, Version=4.5.0.0, Culture=neutral, PublicKeyToken=30ad4fe6b2a6aeed, processorArchitecture=MSIL" />
    <Reference Include="System" />
    <Reference Include="System.Core" />
    <Reference Include="System.Xml.Linq" />
    <Reference Include="System.Data.DataSetExtensions" />
    <Reference Include="Microsoft.CSharp" />
    <Reference Include="System.Data" />
    <Reference Include="System.Net.Http" />
    <Reference Include="System.Xml" />
  </ItemGroup>
  <ItemGroup>
    <Compile Include="${apiCsName conf}" />
    <Compile Include="${converterCsName conf}" />
    <Compile Include="${classCsName conf}" />
    <Compile Include="${enumCsName conf}" />
    <Compile Include="Properties\AssemblyInfo.cs" />
  </ItemGroup>
  <Import Project="$(MSBuildToolsPath)\Microsoft.CSharp.targets" />
  <!-- To modify your build process, add your task inside one of the targets below and uncomment it. 
       Other similar extension points exist, see Microsoft.Common.targets.
  <Target Name="BeforeBuild">
  </Target>
  <Target Name="AfterBuild">
  </Target>
  -->
</Project>|]
