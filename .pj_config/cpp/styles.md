# .clang-format — C++ Code Style Reference

> Based on [Google C++ Style Guide](https://google.github.io/styleguide/cppguide.html)
> with custom overrides. Formatted by [clang-format](https://clang.llvm.org/docs/ClangFormatStyleOptions.html).

---

## Table of Contents

1. [General Settings](#1-general-settings)
2. [Brace Wrapping](#2-brace-wrapping)
3. [Short Forms](#3-short-forms)
4. [Pointer Alignment](#4-pointer-alignment)
5. [Operator Breaking](#5-operator-breaking)
6. [Parameters & Arguments](#6-parameters--arguments)
7. [Include Sorting](#7-include-sorting)
8. [Preprocessor Directives](#8-preprocessor-directives)
9. [Macros](#9-macros)
10. [Comments](#10-comments)

---

## 1. General Settings

| Setting | Value |
|---|---|
| Base style | Google |
| Column limit | 100 |
| Indent width | 2 spaces |
| Tabs | Never |

```cpp
// 2-space indent, 100-column limit
class MyClass {
  void Method() {
    if (condition) {
      DoSomething();
      DoSomethingElse();
    }
  }
};
```

---

## 2. Brace Wrapping

Custom brace wrapping: **functions** get braces on a new line, everything else stays on the same line.

| Context | Brace on new line? |
|---|---|
| Function | Yes |
| Class | No |
| Struct | No |
| Enum | No |
| Control statement (`if`, `for`, `while`) | No |
| `catch` | No |
| `else` | No |

```cpp
// Function — brace on new line
void MyFunction()
{
  // ...
}

// Class — brace on same line
class MyClass {
  int value;
};

// Control statements — brace on same line
if (condition) {
  DoSomething();
} else {
  DoOther();
}

for (int i = 0; i < n; i++) {
  Process(i);
}
```

---

## 3. Short Forms

All short forms are **disabled** — no single-line functions, ifs, or loops.

```cpp
// NOT allowed
void Foo() { return; }
if (x) return;
for (auto &e : list) process(e);

// Required form
void Foo()
{
  return;
}

if (x) {
  return;
}

for (auto &e : list) {
  process(e);
}
```

---

## 4. Pointer Alignment

Pointers and references align to the **right** (next to the variable name).

```cpp
int *ptr;
int &ref;
const std::string &name;
std::unique_ptr<int> *factory;
```

---

## 5. Operator Breaking

Binary and ternary operators break **before** the operator.

```cpp
// Binary operator — break before
int result = longVariableNameA
  + longVariableNameB
  + longVariableNameC;

bool check = conditionA
  && conditionB
  && conditionC;

// Ternary operator — break before
int value = isReady
  ? computeResult()
  : fallbackValue();
```

---

## 6. Parameters & Arguments

- `BinPackParameters: true` — parameters pack onto lines as space allows
- `BinPackArguments: true` — same for call arguments
- `AllowAllParametersOfDeclarationOnNextLine: false` — if the first param fits on the declaration line, it stays there

```cpp
// Short enough — stays on one line
void Foo(int a, int b, int c);

// Too long — bin-packed across lines
void LongFunctionName(int firstParam, int secondParam,
                      int thirdParam, int fourthParam);

// Call site
DoSomething(argumentOne, argumentTwo, argumentThree,
            argumentFour);
```

---

## 7. Include Sorting

Includes are **sorted** and grouped by category:

| Priority | Pattern | Example |
|---|---|---|
| 0 | System/library headers `<...>` | `<stdio.h>`, `<vector>` |
| 1 | Project headers starting with `nvim/` or `vim/` | `"nvim/buffer.h"` |

> **Note:** The `nvim/vim` include category is Neovim-specific. For general C++ projects, you may want to remove or replace this rule.

```cpp
#include <stddef.h>
#include <stdio.h>
#include <string>
#include <vector>

#include "nvim/buffer.h"
#include "nvim/eval.h"
```

---

## 8. Preprocessor Directives

- `IndentPPDirectives: AfterHash` — preprocessor directives indent after the `#`
- `PPIndentWidth: 1` — 1-space indent per nesting level

```cpp
#ifdef USE_FEATURE
# include "feature.h"
# ifdef SUBSYSTEM
#  define FLAG 1
# endif
#endif
```

---

## 9. Macros

- `AlignConsecutiveMacros: AcrossEmptyLines` — consecutive `#define` values align even across blank lines
- `SpaceBeforeParens: ControlStatementsExceptControlMacros` — no space before parens on control macros

```cpp
#define SHORT        1
#define MEDIUM_NAME  2
#define LONGER_NAME  3

#define BUFFER_SIZE  1024
```

### ForEachMacros

> **Note:** These macros are Neovim-specific (`FOR_ALL_BUFFERS`, `RBUFFER_EACH`, etc.). clang-format treats them like `for` loops for indentation. Remove these for non-Neovim projects.

```cpp
FOR_ALL_BUFFERS(buf) {
  process(buf);
}

RBUFFER_EACH(rb, item) {
  handle(item);
}
```

---

## 10. Comments

- `AlignTrailingComments: true` — trailing comments align across consecutive lines
- `SpacesBeforeTrailingComments: 2` — 2 spaces before `//`

```cpp
int x = 1;      // first value
int yy = 2;     // second value
int zzz = 3;    // third value
```

---

## Deprecated Options

The following options in the current `.clang-format` are deprecated since clang-format 19:

| Deprecated | Replacement |
|---|---|
| `AlignEscapedNewlinesLeft` | `AlignEscapedNewlines` (already set) |
| `AlwaysBreakAfterDefinitionReturnType` | `BreakAfterReturnType` |
| `AlwaysBreakAfterReturnType` | `BreakAfterReturnType` |
| `AlwaysBreakBeforeMultilineStrings` | `BreakBeforeMultilineStrings` |
| `AlwaysBreakTemplateDeclarations` | `BreakTemplateDeclarations` |

These still work but will be removed in a future clang-format version.
