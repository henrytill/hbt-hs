{-# LANGUAGE CPP #-}

-- | The version this executable reports through @--version@: the version from
-- the cabal file, followed by the revision the build stamped into @project.h@.
module Version (version) where

import Data.Version (showVersion)
import Paths_hbt_cli qualified as Paths

#include "project.h"

version :: String
version = showVersion Paths.version ++ HBT_COMMIT_SUFFIX
