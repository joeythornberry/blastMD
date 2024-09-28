import Test.HUnit
import qualified System.Exit as Exit
import qualified Data.Map as Map
import Templating (replaceAfterBeginChar, swapBetweenDelimiters, swapWithMap, processTemplate)
import Compilation (readKey, extractParameter, insertSafe, extractMetadataMap, markdownToHtml, processAllTemplates, compileHtml, HtmlCompilationResult (SuccessfulHtmlCompilation))
import Sitemap (createSitemap, processUrl)
import Json (jsonifyValue, jsonifyMap)

assertLeft :: (Show a, Show b) => Either a b -> Assertion
assertLeft (Left _) = return ()
assertLeft x@(Right _) = assertFailure $ show x ++ " is not a Left"

-- JSON.HS

tjsonifyValue :: Test
tjsonifyValue = TestCase $ 
  assertEqual "successful" "\"foo\": \"bar\"" (jsonifyValue "foo" "bar") 

tjsonifyMap :: Test
tjsonifyMap = TestCase $
  assertEqual "successful" "{\n\"baz\": \"buz\",\"foo\": \"bar\"\n}" (jsonifyMap $ Map.fromList [("foo", "bar"), ("baz", "buz")])

-- SITEMAP.HS

tprocessUrl :: Test
tprocessUrl = 
  TestCase $
    assertEqual "successful" (processUrl "https://foo.bar/" "md/hello/goodbye.txt") "https://foo.bar/hello/goodbye.html"

tcreateSitemap :: Test
tcreateSitemap = 
  TestCase $ 
    assertEqual "successful" (createSitemap "https://foo.bar/" ["md/a/b/c.md", "md/d.md"]) "https://foo.bar/a/b/c.html\nhttps://foo.bar/d.html\n"

-- TEMPLATING.HS

treplaceAfterBeginChar :: Test
treplaceAfterBeginChar =
  let result = replaceAfterBeginChar '~' "wo" (\_ -> Right "swapped") "rdhello" in
  TestCase $ do
    assertEqual "successful" (replaceAfterBeginChar '~' "wo" (\_ -> Right "swapped") "rd~hello") $ Right "swappedhello"
    assertLeft result

tswapBetweenDelimiters :: Test
tswapBetweenDelimiters =
  TestCase $ do
    assertEqual "successful" (swapBetweenDelimiters (\_ -> Right "swapped") "hello~word~goodbye") $ Right "helloswappedgoodbye"

tswapWithMap :: Test
tswapWithMap =
  TestCase $ do
    assertEqual "succesful" (swapWithMap (Map.fromList [("word","swapped")]) "word") $ Right "swapped"
    assertLeft (swapWithMap (Map.fromList [("word","swapped")]) "wrongword")

tprocessTemplate :: Test
tprocessTemplate =
  let swapper = swapWithMap (Map.fromList [("w1","s1"), ("w2","s2")]) in
  TestCase $ do
    assertEqual "successful" (processTemplate swapper "hello~w1~middle~w2~goodbye") $ Right "hellos1middles2goodbye"
    assertLeft $ processTemplate swapper "hello~w1"
    assertLeft $ processTemplate swapper "hellow1~middle~w2~goodbye"

-- BLOGCOMPILE.HS
treadKey :: Test
treadKey = 
  TestCase $ do
    assertEqual "successful" (readKey $ Just "Key:") $ Right "Key"
    assertLeft $ readKey Nothing
    assertLeft $ readKey $ Just "Key"

textractParameter :: Test
textractParameter =
  TestCase $ do
    assertEqual "successful" (extractParameter "Key" "Key: Value") $ Right "Value"
    assertLeft $ extractParameter "Key" "Key Value"
    assertLeft $ extractParameter "Key" "Wrong: Value"
    assertLeft $ extractParameter "Key" "Key: "

tinsertSafe :: Test 
tinsertSafe =
  TestCase $ do
    assertEqual "successful" (insertSafe "Key" (Right "Value") Map.empty) $ Map.fromList [("Key","Value")]
    assertEqual "invalid value" (insertSafe "" (Left "errormsg") Map.empty) Map.empty

textractMetadataMap :: Test
textractMetadataMap =
  TestCase $ do
    assertLeft $ extractMetadataMap ["irrelevant"] (Left "errormsg") ["irrelevant"]
    assertEqual "successful" (extractMetadataMap ["k1", "k2"] (Right Map.empty) ["k1: v1", "k2: v2"]) $ Right $ Map.fromList [("k1", "v1"), ("k2", "v2")]

tmarkdownToHtml :: Test
tmarkdownToHtml =
  let str = "<script>alert('xss')</script>" in
  TestCase $ do
    assertEqual "xss protection is off" (markdownToHtml str) str
    assertEqual "markdown rendering works" (markdownToHtml "# hi") "<h1>hi</h1>"

tprocessAllTemplates :: Test
tprocessAllTemplates =
  let templatemap = [("head", "t~p1~head"), ("top", "ttop~p2~"), ("bottom", "~p1~t~p2~bottom")]
      brokentemplatemap = [("head", "t~p1~head"), ("top", "ttopp2~"), ("bottom", "~p1~t~p2~bottom")]
      expectedresult = Right ["tc1head", "ttopc2", "c1tc2bottom"]
      posts = Map.fromList [("p1", "c1"), ("p2", "c2")] in
  TestCase $ do
    assertEqual "successful" (processAllTemplates posts templatemap) expectedresult
    assertLeft $ processAllTemplates posts brokentemplatemap

tcompileHtml :: Test
tcompileHtml =
  let expectedHtml = "<!DOCTYPE html>\n<html>\n<head>\nhec1ad</head>\n<body>\ntop<article>\n<p>content</p>\n</article>\nbottomc1</body>\n</html>" in
  TestCase $ do
    case compileHtml ["m1"] "he~m1~ad" "top" "m1: c1\ncontent" "bottom~m1~" Map.empty of
      Right (SuccessfulHtmlCompilation htmlcontent _) -> assertEqual "Html content is correct" htmlcontent  expectedHtml
      Left errormsg -> assertFailure errormsg

tests :: Test
tests = TestList [
  TestLabel "replaceAfterBeginChar" treplaceAfterBeginChar,
  TestLabel "swapBetweenDelimiters" tswapBetweenDelimiters,
  TestLabel "swapWithMap" tswapWithMap,
  TestLabel "processTemplate" tprocessTemplate,
  TestLabel "readKey" treadKey,
  TestLabel "extractParameter" textractParameter,
  TestLabel "insertSafe" tinsertSafe,
  TestLabel "extractMetadataMap" textractMetadataMap,
  TestLabel "markdownToHtml" tmarkdownToHtml,
  TestLabel "processAllTemplates" tprocessAllTemplates,
  TestLabel "compileHtml" tcompileHtml,
  TestLabel "processUrl" tprocessUrl,
  TestLabel "createSitemap" tcreateSitemap,
  TestLabel "jsonifyValue" tjsonifyValue,
  TestLabel "jsonifyMap" tjsonifyMap
  ]

main :: IO ()
main = do
  count <- runTestTT tests
  if failures count > 0 then Exit.exitFailure else return ()
