// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

base class A {
  var a: i32;
}

base class B {
  extend base: A;
  var b: i32;
}

class C {
  extend base: B;
  var c: i32;
}

fn Main() -> i32 {
  var c: C = {.base = {.base = {.a = 1}, .b = 2}, .c = 3};
  let (pa: A*, pb: B*, pc: C*) = (&c, &c, &c);
  Print("{0}", pa->a);
  Print("{0}", pb->b);
  Print("{0}", pc->c);
  return 0;
}

// CHECK:STDOUT: 1
// CHECK:STDOUT: 2
// CHECK:STDOUT: 3
// CHECK:STDOUT: result: 0
