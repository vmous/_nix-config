// Enable SBT cancel sub-processes with C-c C-v C-c C-c
// http://ensime.github.io/build_tools/sbt/#cancel-processes
cancelable in Global := true

// Do not create directories that are not needed
// https://stackoverflow.com/questions/41070767/ensimeconfig-creates-directories-java-and-scala-2-11-which-i-dont-need
import org.ensime.EnsimeKeys._
ensimeIgnoreMissingDirectories := true
