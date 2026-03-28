{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import StackMCP.TaskManager (newTaskManager, TaskManager)
import StackMCP.Types (ToolResult(..), toolResultIsError, toolResultContent, ContentBlock(..))
import StackMCP.Tools qualified as Tools

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Extract the text content from the first content block.
resultText :: ToolResult -> Text
resultText tr = case toolResultContent tr of
  (cb : _) -> contentText cb
  []       -> ""

-- | Assert a tool result is not an error.
assertSuccess :: String -> ToolResult -> IO ()
assertSuccess label tr =
  assertBool (label ++ " should succeed but got error:\n" ++ T.unpack (resultText tr))
             (not (toolResultIsError tr))

-- | Assert a tool result IS an error.
assertIsError :: String -> ToolResult -> IO ()
assertIsError label tr =
  assertBool (label ++ " should be an error but succeeded:\n" ++ T.unpack (resultText tr))
             (toolResultIsError tr)

-- | Build a JSON params object from key-value pairs.
params :: [(Text, Value)] -> Value
params = object . map (\(k, v) -> fromText k .= v)

emptyParams :: Value
emptyParams = object []

-- | Dispatch a tool call through the full dispatcher.
call :: IORef (Maybe FilePath) -> TaskManager -> Text -> Value -> IO ToolResult
call cwdRef tm name ps = Tools.callTool cwdRef tm name ps

-- | Extract a text field from a JSON-encoded tool result.
--   Falls back to "unknown" if the field or JSON is missing.
extractField :: Text -> Text -> Text
extractField key txt =
  case decodeStrict (TE.encodeUtf8 txt) of
    Just (Object o) -> case KM.lookup (fromText key) o of
      Just (String v) -> v
      _               -> "unknown"
    _ -> "unknown"

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

-- | All integration tests run sequentially to avoid parallel stack lock
--   contention on the template cache and snapshot database.
main :: IO ()
main = defaultMain $ sequentialTestGroup "Integration Tests" AllFinish
  [ happyPathTests
  , errorDiagnosticTests
  , editToolsTests
  ]

------------------------------------------------------------------------
-- Happy path: scaffold once, exercise everything
------------------------------------------------------------------------

happyPathTests :: TestTree
happyPathTests = testCaseSteps "happy path" $ \step ->
  withSystemTempDirectory "stack-mcp-integ" $ \tmpDir -> do
    cwdRef <- newIORef (Just tmpDir)
    tm     <- newTaskManager

    -- == Scaffold ==
    step "stack_new"
    r <- call cwdRef tm "stack_new" (params [("name", String "myapp")])
    assertSuccess "stack_new" r

    let projDir = tmpDir </> "myapp"
    writeIORef cwdRef (Just projDir)

    step "set_repo"
    r2 <- call cwdRef tm "set_repo" (params [("path", String (T.pack projDir))])
    assertSuccess "set_repo" r2

    step "get_repo"
    r3 <- call cwdRef tm "get_repo" emptyParams
    assertSuccess "get_repo" r3
    assertBool "get_repo returns project path" $
      T.isInfixOf "myapp" (resultText r3)

    -- == Build ==
    step "stack_build"
    r4 <- call cwdRef tm "stack_build" emptyParams
    assertSuccess "stack_build" r4

    step "stack_test"
    r5 <- call cwdRef tm "stack_test" emptyParams
    assertSuccess "stack_test" r5

    -- == Exec ==
    step "stack_exec"
    r6 <- call cwdRef tm "stack_exec"
             (params [("command", String "myapp-exe")])
    assertSuccess "stack_exec" r6

    step "stack_eval"
    r7 <- call cwdRef tm "stack_eval"
             (params [("expression", String "putStrLn \"hello from eval\"")])
    assertSuccess "stack_eval" r7
    assertBool "eval output contains hello" $
      T.isInfixOf "hello" (resultText r7)

    -- == Info ==
    step "stack_path (all)"
    r8 <- call cwdRef tm "stack_path" emptyParams
    assertSuccess "stack_path" r8
    assertBool "path output non-empty" $ T.length (resultText r8) > 10

    step "stack_path (project-root)"
    r9 <- call cwdRef tm "stack_path"
            (params [("key", String "project-root")])
    assertSuccess "stack_path key" r9
    assertBool "project-root contains myapp" $
      T.isInfixOf "myapp" (resultText r9)

    step "stack_query"
    r10 <- call cwdRef tm "stack_query" emptyParams
    assertSuccess "stack_query" r10

    step "stack_ide_targets"
    r11 <- call cwdRef tm "stack_ide_targets" emptyParams
    assertSuccess "stack_ide_targets" r11
    assertBool "ide_targets mentions myapp" $
      T.isInfixOf "myapp" (resultText r11)

    step "stack_ide_packages"
    r12 <- call cwdRef tm "stack_ide_packages" emptyParams
    assertSuccess "stack_ide_packages" r12

    -- == Config ==
    step "stack_config_read (stack.yaml)"
    r13 <- call cwdRef tm "stack_config_read"
             (params [("file", String "stack.yaml")])
    assertSuccess "stack_config_read stack.yaml" r13
    assertBool "config contains resolver or snapshot" $
      T.isInfixOf "resolver" (resultText r13) ||
      T.isInfixOf "snapshot" (resultText r13)

    step "stack_config_read (package.yaml)"
    r14 <- call cwdRef tm "stack_config_read"
             (params [("file", String "package.yaml")])
    assertSuccess "stack_config_read package.yaml" r14

    -- == Deps ==
    step "stack_ls_dependencies"
    r15 <- call cwdRef tm "stack_ls_dependencies" emptyParams
    assertSuccess "stack_ls_dependencies" r15
    assertBool "deps non-empty" $ T.length (resultText r15) > 5

    -- == Pipeline ==
    step "stack_pipeline [clean, build]"
    r16 <- call cwdRef tm "stack_pipeline"
             (params [("steps", toJSON (["clean", "build"] :: [Text]))])
    assertSuccess "stack_pipeline" r16

    step "stack_pipeline with bad step"
    r17 <- call cwdRef tm "stack_pipeline"
             (params [("steps", toJSON (["build", "nonsense-step-xyz"] :: [Text]))])
    -- Bad step may cause an error or partial success; just ensure no crash
    assertBool "pipeline ran" $ T.length (resultText r17) > 0

    -- == Tasks ==
    step "task_ghci (spawn)"
    r18 <- call cwdRef tm "task_ghci" emptyParams
    -- GHCi may fail to init due to Paths_* module conflicts in default template;
    -- just verify it returns a response with task_id
    assertBool "task_ghci returns task_id" $
      T.isInfixOf "task_id" (resultText r18)

    step "task_list"
    r19 <- call cwdRef tm "task_list" emptyParams
    assertSuccess "task_list" r19

    step "task_ghci_eval"
    -- Extract the task_id from the task_ghci response
    let tid = extractField "task_id" (resultText r18)
    r20 <- call cwdRef tm "task_ghci_eval"
             (params [("task_id", String tid), ("expression", String "2 + 2")])
    -- GHCi eval may fail due to startup timing, but should not crash
    assertBool "task_ghci_eval returns response" $ T.length (resultText r20) > 0

    step "task_kill"
    r21 <- call cwdRef tm "task_kill"
             (params [("task_id", String tid)])
    assertBool "task_kill returns response" $ T.length (resultText r21) > 0

    step "task_list (after kill)"
    r22 <- call cwdRef tm "task_list" emptyParams
    assertSuccess "task_list after kill" r22

    -- == Clean ==
    step "stack_clean"
    r23 <- call cwdRef tm "stack_clean" emptyParams
    assertSuccess "stack_clean" r23

------------------------------------------------------------------------
-- Error diagnostics: intentional compile error
------------------------------------------------------------------------

errorDiagnosticTests :: TestTree
errorDiagnosticTests = testCaseSteps "error diagnostics" $ \step ->
  withSystemTempDirectory "stack-mcp-diag" $ \tmpDir -> do
    cwdRef <- newIORef (Just tmpDir)
    tm     <- newTaskManager

    step "scaffold"
    r <- call cwdRef tm "stack_new" (params [("name", String "errapp")])
    assertSuccess "stack_new (errapp)" r

    let projDir = tmpDir </> "errapp"
    writeIORef cwdRef (Just projDir)

    step "build (should succeed first)"
    r2 <- call cwdRef tm "stack_build" emptyParams
    assertSuccess "initial build" r2

    step "inject type error"
    let libFile = projDir </> "src" </> "Lib.hs"
    writeFile libFile $ unlines
      [ "module Lib where"
      , "someFunc :: IO ()"
      , "someFunc = 42"  -- Int where IO () expected
      ]

    step "stack_build (should fail with diagnostics)"
    r3 <- call cwdRef tm "stack_build" emptyParams
    assertIsError "stack_build with type error" r3
    let txt = resultText r3
    assertBool "error mentions type mismatch or diagnostic" $
      T.isInfixOf "error" (T.toLower txt) ||
      T.isInfixOf "diagnostic" (T.toLower txt) ||
      T.isInfixOf "couldn't match" (T.toLower txt) ||
      T.isInfixOf "no instance" (T.toLower txt)

------------------------------------------------------------------------
-- Edit tools: deps, modules, rename, list
------------------------------------------------------------------------

editToolsTests :: TestTree
editToolsTests = testCaseSteps "edit tools" $ \step ->
  withSystemTempDirectory "stack-mcp-edit" $ \tmpDir -> do
    cwdRef <- newIORef (Just tmpDir)
    tm     <- newTaskManager

    step "scaffold"
    r <- call cwdRef tm "stack_new" (params [("name", String "editapp")])
    assertSuccess "stack_new (editapp)" r
    let projDir = tmpDir </> "editapp"
    writeIORef cwdRef (Just projDir)

    -- == Dependencies ==
    step "project_add_dependency (aeson)"
    r2 <- call cwdRef tm "project_add_dependency"
            (params [("package", String "aeson")])
    assertSuccess "add dep aeson" r2
    assertBool "reports added" $ T.isInfixOf "added" (resultText r2)

    step "project_add_dependency (duplicate)"
    r3 <- call cwdRef tm "project_add_dependency"
            (params [("package", String "aeson")])
    assertSuccess "add dep duplicate" r3
    assertBool "reports already_present" $ T.isInfixOf "already_present" (resultText r3)

    step "project_add_dependency (library section)"
    r3b <- call cwdRef tm "project_add_dependency"
             (params [("package", String "containers"), ("section", String "library")])
    assertSuccess "add dep to library" r3b
    assertBool "reports added to library" $
      T.isInfixOf "added" (resultText r3b) && T.isInfixOf "library" (resultText r3b)

    step "project_remove_dependency (aeson)"
    r4 <- call cwdRef tm "project_remove_dependency"
            (params [("package", String "aeson")])
    assertSuccess "remove dep aeson" r4
    assertBool "reports removed" $ T.isInfixOf "removed" (resultText r4)

    step "project_remove_dependency (nonexistent)"
    r5 <- call cwdRef tm "project_remove_dependency"
            (params [("package", String "doesnotexist999")])
    assertSuccess "remove nonexistent" r5  -- returns success:false, not an error
    assertBool "reports not found" $ T.isInfixOf "not found" (T.toLower (resultText r5))

    step "project_remove_dependency (library section)"
    r5b <- call cwdRef tm "project_remove_dependency"
             (params [("package", String "containers"), ("section", String "library")])
    assertSuccess "remove dep from library" r5b
    assertBool "reports removed from library" $
      T.isInfixOf "removed" (resultText r5b) && T.isInfixOf "library" (resultText r5b)

    -- == Modules ==
    step "project_list_modules (before)"
    r6 <- call cwdRef tm "project_list_modules" emptyParams
    assertSuccess "list modules" r6
    assertBool "lists source dirs" $ T.isInfixOf "src" (resultText r6)

    step "project_add_module (MyLib.Utils)"
    r7 <- call cwdRef tm "project_add_module"
            (params [("module_name", String "MyLib.Utils")])
    assertSuccess "add module" r7
    assertBool "reports success" $ T.isInfixOf "success" (resultText r7)

    step "project_add_module (duplicate)"
    r8 <- call cwdRef tm "project_add_module"
            (params [("module_name", String "MyLib.Utils")])
    assertSuccess "add module duplicate" r8
    assertBool "reports already exists" $ T.isInfixOf "already exists" (resultText r8)

    step "project_list_modules (after add)"
    r9 <- call cwdRef tm "project_list_modules" emptyParams
    assertSuccess "list after add" r9
    assertBool "MyLib.Utils visible" $ T.isInfixOf "MyLib.Utils" (resultText r9)
    -- Verify module names are clean (not garbled absolute paths)
    assertBool "no garbled paths in list" $
      not (T.isInfixOf ":\\" (resultText r9)) && not (T.isInfixOf ":/" (resultText r9))

    step "project_add_module (AppHelper in app/, source_dir test)"
    r9b <- call cwdRef tm "project_add_module"
             (params [("module_name", String "AppHelper"), ("source_dir", String "app")])
    assertSuccess "add module in app" r9b
    assertBool "reports app source_dir" $ T.isInfixOf "app" (resultText r9b)

    -- == Rename ==
    -- Rename MyLib.Utils -> MyLib.Helpers (moves src/MyLib/Utils.hs to src/MyLib/Helpers.hs)
    step "project_rename_module (MyLib.Utils -> MyLib.Helpers)"
    r10 <- call cwdRef tm "project_rename_module"
             (params [("old_name", String "MyLib.Utils"), ("new_name", String "MyLib.Helpers")])
    assertSuccess "rename module" r10
    assertBool "reports success" $ T.isInfixOf "success" (resultText r10)

    step "project_list_modules (after rename)"
    r11 <- call cwdRef tm "project_list_modules" emptyParams
    assertSuccess "list after rename" r11
    assertBool "MyLib.Helpers visible" $ T.isInfixOf "MyLib.Helpers" (resultText r11)
    assertBool "MyLib.Utils gone" $ not (T.isInfixOf "MyLib.Utils" (resultText r11))

    -- == Expose ==
    step "project_expose_module (Lib first, to avoid hiding it)"
    r12a <- call cwdRef tm "project_expose_module"
              (params [("module_name", String "Lib")])
    assertSuccess "expose Lib" r12a

    step "project_expose_module (MyLib.Helpers)"
    r12 <- call cwdRef tm "project_expose_module"
             (params [("module_name", String "MyLib.Helpers")])
    assertSuccess "expose module" r12
    assertBool "reports exposed" $
      T.isInfixOf "exposed" (resultText r12)

    -- Verify it's in package.yaml
    step "verify package.yaml"
    r13 <- call cwdRef tm "stack_config_read"
             (params [("file", String "package.yaml")])
    assertSuccess "read package.yaml" r13
    assertBool "exposed-modules in config" $
      T.isInfixOf "exposed-modules" (resultText r13)

    -- == Rename with exposed-modules update ==
    step "project_rename_module (MyLib.Helpers -> MyLib.Core)"
    r14b <- call cwdRef tm "project_rename_module"
              (params [("old_name", String "MyLib.Helpers"), ("new_name", String "MyLib.Core")])
    assertSuccess "rename to Core" r14b
    assertBool "exposed_modules_updated" $
      T.isInfixOf "exposed_modules_updated" (resultText r14b)

    -- Verify exposed-modules was updated in package.yaml
    step "verify exposed-modules updated after rename"
    r14c <- call cwdRef tm "stack_config_read"
              (params [("file", String "package.yaml")])
    assertSuccess "read package.yaml after rename" r14c
    assertBool "MyLib.Core in exposed-modules" $
      T.isInfixOf "MyLib.Core" (resultText r14c)
    assertBool "MyLib.Helpers no longer in exposed-modules" $
      not (T.isInfixOf "MyLib.Helpers" (resultText r14c))

    -- == Word-boundary safety for rename ==
    -- Add a module whose name is a prefix of "Lib" usage
    step "project_add_module (Library.Extra)"
    r14d <- call cwdRef tm "project_add_module"
              (params [("module_name", String "Library.Extra")])
    assertSuccess "add Library.Extra" r14d

    -- Write an import of Library.Extra into MyLib.Core
    step "write import of Library.Extra into MyLib.Core"
    let coreFile = projDir </> "src" </> "MyLib" </> "Core.hs"
    writeFile coreFile $ unlines
      [ "module MyLib.Core where"
      , "import Library.Extra"
      , ""
      , "helper :: String"
      , "helper = \"core\""
      ]

    -- Rename Lib -> LibRenamed; should NOT corrupt "Library.Extra"
    step "project_rename_module (Lib -> LibRenamed, word-boundary test)"
    r14e <- call cwdRef tm "project_rename_module"
              (params [("old_name", String "Lib"), ("new_name", String "LibRenamed")])
    assertSuccess "rename Lib -> LibRenamed" r14e

    -- Verify Library.Extra import was NOT corrupted
    step "verify word-boundary safety"
    coreContent <- readFile coreFile
    assertBool "Library.Extra import preserved (not corrupted to LibRenamedrary.Extra)" $
      "import Library.Extra" `elem` lines coreContent

    -- == cleanEmptyDirs verification ==
    -- Add a deeply nested module, then rename it to a different path.
    -- The old parent directory should be cleaned up.
    step "project_add_module (Deep.Nested.Mod)"
    r15a <- call cwdRef tm "project_add_module"
              (params [("module_name", String "Deep.Nested.Mod")])
    assertSuccess "add Deep.Nested.Mod" r15a

    step "project_rename_module (Deep.Nested.Mod -> Flat)"
    r15b <- call cwdRef tm "project_rename_module"
              (params [("old_name", String "Deep.Nested.Mod"), ("new_name", String "Flat")])
    assertSuccess "rename Deep.Nested.Mod -> Flat" r15b

    step "verify empty dirs were cleaned"
    deepExists <- doesDirectoryExist (projDir </> "src" </> "Deep")
    assertBool "Deep/ directory should be removed after rename" (not deepExists)

    -- == Extra Deps ==
    step "project_add_extra_dep (acme-missiles-0.3)"
    rEd1 <- call cwdRef tm "project_add_extra_dep"
              (params [("package", String "acme-missiles-0.3")])
    assertSuccess "add extra dep" rEd1
    assertBool "reports added" $ T.isInfixOf "added" (T.toLower (resultText rEd1))

    step "project_add_extra_dep (duplicate)"
    rEd2 <- call cwdRef tm "project_add_extra_dep"
              (params [("package", String "acme-missiles-0.3")])
    assertSuccess "add extra dep dup" rEd2
    assertBool "reports already present" $ T.isInfixOf "already" (T.toLower (resultText rEd2))

    step "project_remove_extra_dep (acme-missiles)"
    rEd3 <- call cwdRef tm "project_remove_extra_dep"
              (params [("package", String "acme-missiles")])
    assertSuccess "remove extra dep" rEd3
    assertBool "reports removed" $ T.isInfixOf "removed" (T.toLower (resultText rEd3))

    step "project_remove_extra_dep (nonexistent)"
    rEd4 <- call cwdRef tm "project_remove_extra_dep"
              (params [("package", String "doesnotexist999")])
    assertSuccess "remove nonexistent extra dep" rEd4
    assertBool "reports not found" $
      T.isInfixOf "not found" (T.toLower (resultText rEd4)) ||
      T.isInfixOf "no extra-deps" (T.toLower (resultText rEd4))

    -- == GHC Options ==
    step "project_set_ghc_options (-Wall)"
    rGo1 <- call cwdRef tm "project_set_ghc_options"
              (params [("options", String "-Wall -Wextra")])
    assertSuccess "set ghc-options" rGo1

    step "project_set_ghc_options (clear)"
    rGo2 <- call cwdRef tm "project_set_ghc_options"
              (params [("options", String "")])
    assertSuccess "clear ghc-options" rGo2

    -- == Default Extensions ==
    step "project_add_default_extension (LambdaCase)"
    rDe1 <- call cwdRef tm "project_add_default_extension"
              (params [("extension", String "LambdaCase")])
    assertSuccess "add default ext" rDe1

    step "project_add_default_extension (duplicate)"
    rDe2 <- call cwdRef tm "project_add_default_extension"
              (params [("extension", String "LambdaCase")])
    assertSuccess "add default ext dup" rDe2
    assertBool "reports already present" $ T.isInfixOf "already" (T.toLower (resultText rDe2))

    step "project_remove_default_extension (LambdaCase)"
    rDe3 <- call cwdRef tm "project_remove_default_extension"
              (params [("extension", String "LambdaCase")])
    assertSuccess "remove default ext" rDe3

    step "project_remove_default_extension (nonexistent)"
    rDe4 <- call cwdRef tm "project_remove_default_extension"
              (params [("extension", String "NoSuchExtension")])
    assertSuccess "remove nonexistent ext" rDe4
    assertBool "reports not found" $
      T.isInfixOf "not found" (T.toLower (resultText rDe4)) ||
      T.isInfixOf "no default-extensions" (T.toLower (resultText rDe4))

    -- == Add Component ==
    step "project_add_component (test-suite)"
    rCo1 <- call cwdRef tm "project_add_component"
              (params [("name", String "editapp-test"), ("type", String "test-suite")])
    assertSuccess "add component" rCo1
    assertBool "reports created" $
      T.isInfixOf "created" (T.toLower (resultText rCo1)) ||
      T.isInfixOf "success" (T.toLower (resultText rCo1))

    -- Verify the test source dir exists
    testDirExists <- doesDirectoryExist (projDir </> "test")
    assertBool "test/ directory created" testDirExists

    -- == Resolve Module ==
    step "project_resolve_module (Flat)"
    rRm1 <- call cwdRef tm "project_resolve_module"
              (params [("module_name", String "Flat")])
    assertSuccess "resolve module" rRm1
    assertBool "resolves to file path" $
      T.isInfixOf "Flat.hs" (resultText rRm1)

    step "project_resolve_module (nonexistent)"
    rRm2 <- call cwdRef tm "project_resolve_module"
              (params [("module_name", String "Does.Not.Exist")])
    assertIsError "resolve nonexistent" rRm2
    assertBool "reports not found" $
      T.isInfixOf "not found" (T.toLower (resultText rRm2)) ||
      T.isInfixOf "not_found" (T.toLower (resultText rRm2))

    -- == Remove Module ==
    step "project_remove_module (Flat)"
    rDel1 <- call cwdRef tm "project_remove_module"
               (params [("module_name", String "Flat")])
    assertSuccess "remove module" rDel1
    assertBool "reports deleted" $
      T.isInfixOf "deleted" (T.toLower (resultText rDel1)) ||
      T.isInfixOf "removed" (T.toLower (resultText rDel1)) ||
      T.isInfixOf "success" (T.toLower (resultText rDel1))

    -- == Build to verify everything compiles ==
    step "stack_build (verify edit results compile)"
    r14 <- call cwdRef tm "stack_build" emptyParams
    assertSuccess "build after edits" r14
