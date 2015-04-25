import Test.Tasty
import qualified Data.Matrix.Generic as MG
import qualified Data.Matrix.Dense.Generic as MD
import qualified Data.Matrix.Sparse.Generic as MS
import qualified Data.Vector.Unboxed as U
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Main"
    [ testCase "xx" testEqual ]


testEqual :: Assertion
testEqual = do
    let xs = [0,0,0,0,1,2,3,0,0,0,0,0,4,5,67,0,0,2,40,0,2,0,0,20,0,0,0]
        m1 = MG.fromList (3,9) xs :: MD.Matrix U.Vector Int
        al = filter ((/=0) . snd) $ MD.toList $ MD.imap (\i v -> (i,v)) m1
        m2 = MG.fromList (3,9) xs :: MS.CSR U.Vector Int
        m3 = MS.fromAscAL (3,9) (length al) al :: MS.CSR U.Vector Int

        row1 = MG.toRows m1
        row2 = MG.toRows m2
    
--    assertEqual "x" (MG.flatten m1) (MG.flatten m2)
    assertEqual "x" (MG.flatten m2) (MG.flatten m3)
    assertEqual "x" row1 row2

