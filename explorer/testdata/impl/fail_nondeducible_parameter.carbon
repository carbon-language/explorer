// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// NOAUTOUPDATE

package ExplorerTest api;

interface A(T:! type) {}
interface B {}

// CHECK:STDERR: COMPILATION ERROR: fail_nondeducible_parameter.carbon:[[@LINE+1]]: parameter `T:! type` is not deducible from `impl i32 as interface B`
impl forall [T:! type] i32 as A(T) & B {}

fn Main() -> i32 { return 0; }
