-- Executalbe programs are compiled based on an entrypoint file that has to
-- contain the `main` function and must not have a `mod` declaration.

use core:io as io;
use lib;

def main := do
  io:println "Resistance is futile! Your actions don't matter...";
  let name := io:input "Enter your name: "
  in lib:greet (lib:first lib:magic name);
end

