module GhcPlugins.Template ( plugin ) where

import GhcPlugins.Template.Pass
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = do
    reinitializeGlobals
    return $ CoreDoPluginPass "Template" transformProgram : todos
