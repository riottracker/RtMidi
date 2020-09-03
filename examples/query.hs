import Sound.RtMidi (closeDevice, compiledApis, createOutput, currentApi, defaultOutput, portCount, portName)

main :: IO ()
main = do
  device <- defaultOutput
  api <- currentApi device
  builtin <- compiledApis
  numPorts <- portCount device
  portNames <- mapM (portName device) [0..numPorts-1]
  putStrLn $ "RtMidi output using " ++ (show api)
  putStrLn $ "built-in: " ++ (show builtin)
  putStrLn $ "available Ports: " ++ (show numPorts)
  putStrLn (show portNames)
  closeDevice device
