module Install where

import Shake

type InstallDir = FilePath

withInstallDir :: InstallDir -> (InstallDir -> Rules a) -> Rules a
withInstallDir name mkRules = (applyEnv $ "${INSTALL_DIR}" </> name) >>= mkRules
