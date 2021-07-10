-- Executalbe programs are compiled based on an entrypoint file that has to
-- contain the `mod main` declaration and the `main` function.
mod main;

use core:io as io;
use lib;

def main := do
  io:println "Resistance is futile! Your actions don't matter...";
  let name := io:input "Enter your name: "
  in lib:greet (lib:first lib:magic name);
end

