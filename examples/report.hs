import Sound.RtMidi.Report (buildReport)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = buildReport False >>= pPrint
