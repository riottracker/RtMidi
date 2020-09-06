import Sound.RtMidi (closeDevice, compiledApis, createOutput, currentApi, defaultOutput, listPorts)

main :: IO ()
main = do
  device <- defaultOutput
  api <- currentApi device
  builtin <- compiledApis
  portPairs <- listPorts device
  putStrLn $ "RtMidi output using " ++ (show api)
  putStrLn $ "built-in: " ++ (show builtin)
  putStrLn $ "available ports: " ++ (show portPairs)
  closeDevice device
