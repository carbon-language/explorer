// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

interface Foo {}
alias A = Foo;

fn Main() -> i32 {
    // CHECK:STDERR: COMPILATION ERROR: fail_wrong_interface_alias_use.carbon:[[@LINE+1]]: member access, bar not in interface Foo
    return A.bar;
}
