{-# LANGUAGE CPP #-}

-- | The version this executable reports through @--version@: the version from
-- the cabal file, and the revision it was built from when the build stamped one
-- into @project.h@.
module Version (version) where

import Data.Version qualified as Version
import Paths_hbt_cli qualified as Paths

#include "project.h"

version :: String
#ifdef HBT_COMMIT
version = Version.showVersion Paths.version ++ "-" ++ HBT_COMMIT
#else
version = Version.showVersion Paths.version
#endif
