// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

fn Main() -> i32 {
  var s: String = '''
    A "block" ""string"" literal
      with indent.
    ''';
  if (s == "A \"block\" \"\"string\"\" literal\n  with indent.\n") {
    return 0;
  } else {
    return 1;
  }
}

// CHECK:STDOUT: result: 0
