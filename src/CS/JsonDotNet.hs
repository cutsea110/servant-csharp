{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CS.JsonDotNet ( apiCsForAPI
                     , apiCsForAPIWith
                     , enumCsForAPI
                     , enumCsForAPIWith

                     , GenerateCsConfig(..)
                     , def
                     ) where

import Prelude hiding (concat, lines, unlines)
import Control.Arrow
import Control.Lens
import Data.ByteString (ByteString)
import Data.ByteString.Char8 as BC (unpack)
import Data.Char (toUpper, toLower)
import Data.List (intercalate, concat)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text as T (Text, unpack, pack)
import Language.Haskell.Exts
import Servant.Foreign
import Text.Heredoc

import CS.Common (CSharp, getEndpoints)

data GenerateCsConfig
    = GenerateCsConfig { namespace :: String
                       , apitemplate ::  forall api.
                                         (HasForeign CSharp Text api,
                                          GenerateList Text (Foreign Text api))
                                         => GenerateCsConfig
                                     -> Proxy api
                                     -> IO String
                       , enumtemplate :: GenerateCsConfig -> IO String
                       , sources :: [FilePath]
                       }

def :: GenerateCsConfig
def = GenerateCsConfig { namespace = "ServantClientAPI"
                       , apitemplate = defAPITemplate
                       , enumtemplate = defEnumTemplate
                       , sources = []
                       }

--------------------------------------------------------------------------
isEnumLikeDataDecl :: Decl -> Bool
isEnumLikeDataDecl (DataDecl _ DataType _ _ _ xs _)
    = all isEnumLikeConDecl xs
isEnumLikeDataDecl _ = False

isEnumLikeConDecl :: QualConDecl -> Bool
isEnumLikeConDecl (QualConDecl _ _ _ (ConDecl _ [])) = True
isEnumLikeConDecl _ = False

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

origType :: QualConDecl -> String
origType (QualConDecl _ _ _ (RecDecl _ [(_, TyCon (UnQual (Ident t)))]))
    = case t of
        "String"  -> "System.String"
        "Text"    -> "System.String"
        "Int"     -> "System.Int64"
        "Integer" -> "System.Int64"
        t         -> error "don't supported type. "<>t

usingAliasesFromFiles :: [FilePath] -> IO [(String, String)]
usingAliasesFromFiles hss
    = return . concat =<< mapM usingAliasesFromFile hss

usingAliasesFromFile :: FilePath -> IO [(String, String)]
usingAliasesFromFile hs = do
  ParseOk (Module _ _ _ _ _ _ decls) <- parseFile hs
  let xs = filter isNewtypeDecl decls
  return $ map toTuple xs
    where
      toTuple (DataDecl _ NewType _ (Ident name) _ [qcon] _)
              = (name, origType qcon)

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

defAPITemplate :: (HasForeign CSharp Text api,
                GenerateList Text (Foreign Text api)) =>
               GenerateCsConfig -> Proxy api -> IO String
defAPITemplate conf api = do
  usingAliases <- usingAliasesFromFiles $ sources conf
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
$forall (n, t) <- usingAliases
  using ${n} = ${t};
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
  es <- enumTypesFromFiles $ sources conf
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

