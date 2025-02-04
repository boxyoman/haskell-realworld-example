import System.FilePath.Glob (glob)
import Test.DocTest (doctest)
import Test.Hspec
import Test.Hspec (hspec)
import Test.Hspec.QuickCheck
import Types qualified as T




main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hspec $ do
    describe "slug" $ do
      it "title to slug" $ do
        let title = "Hello World"
        let slug = "hello-world"
        T.mkSlug (T.Title title) `shouldBe` T.Slug slug
