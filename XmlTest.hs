module XmlTest where

import Test.QuickCheck
import Text.XML.Light.Input
import Text.XML.Light.Types
import Features
import XmlHelper

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

parseTest = case parseXMLDoc testData of
                 Nothing -> False
                 Just _  -> True

parseTrees xml = let x = parseRequest xml in True
