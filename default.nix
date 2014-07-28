{ cabal, attoparsec, base64Bytestring, blazeBuilder, blazeHtml
, caseInsensitive, conduit, dns, errors, exceptions, filepath
, hledgerLib, hslogger, httpTypes, HUnit, lens, mtl, network
, pandoc, processConduit, QuickCheck, quickcheckIo, semigroups
, testFramework, testFrameworkQuickcheck2, testFrameworkTh, text
, time, transformers, wai, waiTest, warp, yaml
}:

cabal.mkDerivation (self: {
  pname = "maison";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    attoparsec base64Bytestring blazeBuilder blazeHtml caseInsensitive
    conduit dns errors exceptions filepath hledgerLib hslogger
    httpTypes lens mtl network pandoc processConduit semigroups text
    time transformers wai warp yaml
  ];
  testDepends = [
    httpTypes HUnit lens QuickCheck quickcheckIo testFramework
    testFrameworkQuickcheck2 testFrameworkTh text wai waiTest
  ];
  meta = {
    description = "Personal web server";
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
