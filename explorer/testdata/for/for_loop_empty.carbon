// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;


fn Main() -> i32 {
  var ar: [i32; 0] = () ;
  var count : i32 = 0;
  for( x: i32 in ar ){
      count = 2;
  }
  return count;
}

// CHECK:STDOUT: result: 0
