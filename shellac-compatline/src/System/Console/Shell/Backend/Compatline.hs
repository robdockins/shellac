{-

   Copyright 2008, Robert Dockins.

 -}

{- | This module implements a Shellac backend based either on
     GNU readline or on libedit.  The choice between these two
     packages is made at compile time, based on availability.  In the case that
     both are available, libedit is chosen.

     Beware that while the code for this Shellac binding is licensed under a BSD3
     license, GNU readline itself is licensed under the GPL.  This means that your
     project needs to be GPL compatible to use this backend!  Otherwise you may encounter
     licensing issues.

     If your project is not GPL compatible you should instead use the Shellac-editline
     library, as editline is licensed under a BSD3 license.
-}

module System.Console.Shell.Backend.Compatline where

import System.Console.Shell.Backend

#ifdef USE_EDITLINE
import System.Console.Shell.Backend.Editline
#endif

#ifdef USE_READLINE
import System.Console.Shell.Backend.Readline
#endif

data CompatlineConfig
  = UsingEditline
  | UsingReadline
 deriving Show

-- | A \"readline-alike\" shell backend.
compatlineBackend :: ShellBackend ()

-- | A flag describing the compile-time
--   configuration of this module.
compatlineConfig :: CompatlineConfig


#ifdef USE_EDITLINE
compatlineBackend = editlineBackend
compatlineConfig = UsingEditline
#endif


#ifdef USE_READLINE
compatlineBackend = readlineBackend
compatlineConfig = UsingReadline
#endif
