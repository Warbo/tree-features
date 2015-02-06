import Data.Char
import Data.Hash.MD5
import Data.Maybe
import System.IO
import Text.XML.Light.Input
import Text.XML.Light.Types

name :: Element -> String
name = map toLower . qName . elName

countLeaves :: Element -> Int
countLeaves = undefined

newtype Tree = T (Int, [Tree]) -- Feature vector and sub-trees

instance Show Tree where
  show (T (x, xs)) = "(" ++ show x ++ " " ++ show xs ++ ")"

type Request = (Tree, [Tree])  -- Goal and context

parseRequest :: Element -> Request
parseRequest e = let (goal:context) = elContent e
                 in (parseTerm goal, map parseTerm context)

parseTerm :: Content -> Tree
parseTerm (Elem e) = let subtrees = map parseTerm (elContent e)
                         name     = qName (elName e)
                         attrs    = map attrStr (elAttribs e)
                         fVec     = features . map feature $ name : attrs
                     in T (fVec, subtrees)
parseTerm (Text t) = T (feature (cdData t), [])

attrStr :: Attr -> String
attrStr a = qName (attrKey a) ++ attrVal a

-- FIXME: Temporary, need a nice dimension-reducer
features :: [Int] -> Int
features = foldr (+) 0

-- FIXME: Temporary, need to limit this to N bits
feature :: String -> Int
feature s = fromIntegral . md5i $ (Str s)

testData = "<REQUEST req=\"\">\
            \  <APPLY id=\"i0\" sort=\"Type\">\
            \    <MUTIND uri=\"cic:/Coq/Init/Logic/eq.ind\" noType=\"0\" id=\"i67\"/>\
            \    <MUTIND uri=\"cic:/Coq/Init/Datatypes/nat.ind\" noType=\"0\" id=\"i64\"/>\
            \    <APPLY id=\"i38\" sort=\"Set\">\
            \      <MUTCONSTRUCT uri=\"cic:/Coq/Init/Datatypes/nat.ind\" noType=\"0\" noConstr=\"2\" id=\"i60\" sort=\"Set\"/>\
            \      <APPLY id=\"i40\" sort=\"Set\">\
            \        <MUTCONSTRUCT uri=\"cic:/Coq/Init/Datatypes/nat.ind\" noType=\"0\" noConstr=\"2\" id=\"i56\" sort=\"Set\"/>\
            \        <APPLY id=\"i42\" sort=\"Set\">\
            \          <MUTCONSTRUCT uri=\"cic:/Coq/Init/Datatypes/nat.ind\" noType=\"0\" noConstr=\"2\" id=\"i52\" sort=\"Set\"/>\
            \          <APPLY id=\"i44\" sort=\"Set\">\
            \            <MUTCONSTRUCT uri=\"cic:/Coq/Init/Datatypes/nat.ind\" noType=\"0\" noConstr=\"2\" id=\"i48\" sort=\"Set\"/>\
            \            <MUTCONSTRUCT uri=\"cic:/Coq/Init/Datatypes/nat.ind\" noType=\"0\" noConstr=\"1\" id=\"i46\" sort=\"Set\"/>\
            \          </APPLY>\
            \        </APPLY>\
            \      </APPLY>\
            \    </APPLY>\
            \    <APPLY id=\"i2\" sort=\"Set\">\
            \      <CONST uri=\"cic:/Coq/Init/Peano/plus.con\" id=\"i32\" sort=\"Set\"/>\
            \      <APPLY id=\"i12\" sort=\"Set\">\
            \        <MUTCONSTRUCT uri=\"cic:/Coq/Init/Datatypes/nat.ind\" noType=\"0\" noConstr=\"2\" id=\"i28\" sort=\"Set\"/>\
            \        <APPLY id=\"i14\" sort=\"Set\">\
            \          <MUTCONSTRUCT uri=\"cic:/Coq/Init/Datatypes/nat.ind\" noType=\"0\" noConstr=\"2\" id=\"i24\" sort=\"Set\"/>\
            \          <APPLY id=\"i16\" sort=\"Set\">\
            \            <MUTCONSTRUCT uri=\"cic:/Coq/Init/Datatypes/nat.ind\" noType=\"0\" noConstr=\"2\" id=\"i20\" sort=\"Set\"/>\
            \            <MUTCONSTRUCT uri=\"cic:/Coq/Init/Datatypes/nat.ind\" noType=\"0\" noConstr=\"1\" id=\"i18\" sort=\"Set\"/>\
            \          </APPLY>\
            \        </APPLY>\
            \      </APPLY>\
            \      <APPLY id=\"i4\" sort=\"Set\">\
            \        <MUTCONSTRUCT uri=\"cic:/Coq/Init/Datatypes/nat.ind\" noType=\"0\" noConstr=\"2\" id=\"i8\" sort=\"Set\"/>\
            \        <MUTCONSTRUCT uri=\"cic:/Coq/Init/Datatypes/nat.ind\" noType=\"0\" noConstr=\"1\" id=\"i6\" sort=\"Set\"/>\
            \      </APPLY>\
            \    </APPLY>\
            \  </APPLY>\
            \  <APPLY id=\"i0\" sort=\"Type\">\
            \    <MUTIND uri=\"cic:/Coq/Init/Datatypes/list.ind\" noType=\"0\" id=\"i5\"/>\
            \    <MUTIND uri=\"cic:/Coq/Init/Datatypes/bool.ind\" noType=\"0\" id=\"i2\"/>\
            \  </APPLY>\
            \</REQUEST>"

parse = fromJust . parseXMLDoc

getXml = fmap parse getContents

main = fmap parseXMLDoc getContents >>= print
