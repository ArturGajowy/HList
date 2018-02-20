{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- tests based on HListExample
module Main (main) where


import Test.Hspec


import HListExample.Labelable
import HListExample.CmdArgs
import HListExample.MainGhcGeneric1
-- import HListExample.MainPosting040607
import HListExample.MainPosting051106
import HListExample.Prism
import HListExample.Pun
import HListExample.TIPTransform
import HListExample.TIPTransformM


main = hspec $ do
  mainCmdargs
  mainLabelable
  mainGhcGeneric1
  mainPosting051106
  mainPrism
  mainPun
  mainTIPTransform
  mainTTIPM






