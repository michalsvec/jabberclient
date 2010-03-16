--
-- Project: jabclient
-- Author:  XXX
--

-- | Main project module.
module Main where

import Qt

main :: IO Int
main = do
  qApplication ()
  hello <- qPushButton "Hello qtHaskell World"
  resize hello (200::Int, 60::Int)
  qshow hello ()
  qApplicationExec ()

-- End of file
