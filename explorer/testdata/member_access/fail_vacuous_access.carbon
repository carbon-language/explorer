// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package Foo api;
interface A { fn F() -> i32; }
class X {
  extend impl as A {
    fn F() -> i32 { return 1; }
  }
}
fn Main() -> i32 {
  var a: X = {};
  // CHECK:STDERR: COMPILATION ERROR: fail_vacuous_access.carbon:[[@LINE+1]]: expected name of instance member or interface member in compound member access, found fn () -> i32
  return a.(X.(A.F))();
}
