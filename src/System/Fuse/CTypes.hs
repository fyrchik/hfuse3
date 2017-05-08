module System.Fuse.CTypes
-- THIS MODULE MUST NOT IMPORT ANY OTHERS
-- IT IS PURELY DECLARATIVE
    ( CDirHandle
    , CFillDirBuf
    , CFuseArgs
    , CFuseChan
    , CFuseCmdlineOpts
    , CFuseConnInfo
    , CFuseContext
    , CFuseFileInfo
    , CFuseOperations
    , CFuseOpt
    , CFuseSession
    , CStructFuse
    , CStructStatVFS

    , CStat


    ) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

-- | struct types from FUSE
data CDirHandle       -- fuse_dirh_t
data CFillDirBuf      -- void
data CFuseArgs        -- struct fuse_args
data CFuseChan        -- struct fuse_chan
data CFuseCmdlineOpts -- struct fuse_cmdline_opts
data CFuseConnInfo    -- struct fuse_conn_info
data CFuseContext     -- struct fuse_context
data CFuseFileInfo    -- struct fuse_file_info
data CFuseOperations  -- struct fuse_operations
data CFuseOpt         -- struct fuse_opt
data CFuseSession     -- struct fuse_session
data CStructFuse      -- struct fuse
data CStructStatVFS   -- struct fuse_stat_fs

-- | types from other headers
data CStat -- struct stat (stat.h)
