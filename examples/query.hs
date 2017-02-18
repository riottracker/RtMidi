import Sound.RtMidi
import Control.Concurrent

main = do
  device <- defaultOutput
  asize <- apiSize
  api <- currentApi device
  builtin <- compiledApis
  numPorts <- portCount device
  portNames <- mapM (portName device) [0..numPorts-1]
  putStrLn $ "RtMidi output using " ++ (show api) ++ " (" ++ (show asize) ++ ")"
  putStrLn $ "built-in: " ++ (show builtin)
  putStrLn $ "available Ports: " ++ (show numPorts)
  putStrLn (show portNames)
  closeOutput device
