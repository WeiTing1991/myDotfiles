# .editorconfig — Microsoft .NET Conventions

> Based on [Microsoft .NET code-style rule options](https://learn.microsoft.com/dotnet/fundamentals/code-analysis/code-style-rule-options)
> and [.NET naming guidelines](https://learn.microsoft.com/dotnet/standard/design-guidelines/naming-guidelines).

---

## Table of Contents

1. [Global Defaults](#1-global-defaults)
2. [File-Type Overrides](#2-file-type-overrides)
3. [C# — Using Directives](#3-c-using-directives)
4. [C# — this. Qualification](#4-c-this-qualification)
5. [C# — Language Keywords vs BCL Types](#5-c-language-keywords-vs-bcl-types)
6. [C# — var Preferences](#6-c-var-preferences)
7. [C# — Expression-Bodied Members](#7-c-expression-bodied-members)
8. [C# — Pattern Matching](#8-c-pattern-matching)
9. [C# — Null Checking](#9-c-null-checking)
10. [C# — Modern Language Features](#10-c-modern-language-features)
11. [C# — Modifier Preferences](#11-c-modifier-preferences)
12. [C# — Braces](#12-c-braces)
13. [C# — New Lines (Allman Style)](#13-c-new-lines-allman-style)
14. [C# — Indentation](#14-c-indentation)
15. [C# — Spacing](#15-c-spacing)
16. [C# — Wrapping & Layout](#16-c-wrapping--layout)
17. [Naming Conventions](#17-naming-conventions)
18. [Diagnostic Severities](#18-diagnostic-severities)
19. [File-Specific Overrides](#19-file-specific-overrides)

---

## 1. Global Defaults

Applies to **all files** (`[*]`).

| Setting | Value | Purpose |
|---|---|---|
| `charset` | `utf-8` | Ensures consistent encoding across platforms |
| `indent_style` | `space` | Uses spaces, not tabs |
| `indent_size` | `4` | 4-space indentation unit |
| `tab_width` | `4` | Renders stray tabs at 4 spaces wide |
| `end_of_line` | `lf` | Unix line endings — avoids CRLF issues in Git |
| `insert_final_newline` | `true` | POSIX standard — avoids diff noise on last line |
| `trim_trailing_whitespace` | `true` | Strips trailing spaces on save |
| `max_line_length` | `179` | Soft line length limit used by linters |
| `guidelines` | `80, 120` | Visual ruler lines at 80 and 120 chars |
| `guidelines_style` | `1px dotted gray` | Renders rulers as faint dotted lines in the editor |

---

## 2. File-Type Overrides

Certain file types deviate from the global defaults.

| Pattern | Setting | Value | Reason |
|---|---|---|---|
| `*.{xml,csproj,props,…}` | `indent_size` | `2` | XML tends to be deeply nested |
| `*.{json,yml,yaml}` | `indent_size` | `2` | Community standard for config files |
| `*.md` | `indent_size` | `2` | Standard Markdown indentation |
| `*.md` | `trim_trailing_whitespace` | `false` | Two trailing spaces = intentional line break in Markdown |
| `*.sln` | `indent_style` | `tab` | Visual Studio generates solution files with real tabs |
| `*.ps1` | `indent_size` | `2` | PowerShell convention |

---

## 3. C# — Using Directives

| Setting | Value | Effect |
|---|---|---|
| `dotnet_sort_system_directives_first` | `true` | `System.*` usings appear before all others |
| `dotnet_separate_import_directive_groups` | `false` | No blank lines between using groups |
| `csharp_using_directive_placement` | `outside_namespace:warning` | Usings go outside the namespace block |
| `IDE0005` | `warning` | Warns on unnecessary / unused using directives |

---

## 4. C# — `this.` Qualification

Prefer **not** qualifying members with `this.` — keeps code concise.

Applies to: fields, properties, methods, and events. Enforced as a `suggestion`.
```csharp
// ✅ Preferred
_name = value;

// ❌ Avoid
this._name = value;
```

---

## 5. C# — Language Keywords vs BCL Types

Prefer language keywords over BCL type names. Enforced as a `suggestion`.
```csharp
// ✅ Preferred
int count = 0;
string name = "foo";

// ❌ Avoid
Int32 count = 0;
String name = "foo";
```

---

## 6. C# — `var` Preferences

| Scenario | Setting | Value |
|---|---|---|
| Built-in types (`int`, `string`, …) | `csharp_style_var_for_built_in_types` | `false` — use explicit type |
| Type is apparent from the right-hand side | `csharp_style_var_when_type_is_apparent` | `true` — use `var` |
| All other cases | `csharp_style_var_elsewhere` | `false` — use explicit type |
```csharp
// ✅ Explicit for built-ins
int count = 5;

// ✅ var when type is obvious
var orders = new List<Order>();

// ✅ Explicit when type is ambiguous
OrderService service = GetService();
```

---

## 7. C# — Expression-Bodied Members

| Member type | Setting | Preference |
|---|---|---|
| Methods | `csharp_style_expression_bodied_methods` | `false` — block body |
| Constructors | `csharp_style_expression_bodied_constructors` | `false` — block body |
| Operators | `csharp_style_expression_bodied_operators` | `false` — block body |
| Properties | `csharp_style_expression_bodied_properties` | `true` — `=>` when single line |
| Indexers | `csharp_style_expression_bodied_indexers` | `true` — `=>` when single line |
| Accessors | `csharp_style_expression_bodied_accessors` | `true` — `=>` when single line |
| Lambdas | `csharp_style_expression_bodied_lambdas` | `true` — no enforcement (`silent`) |
| Local functions | `csharp_style_expression_bodied_local_functions` | `false` — block body |
```csharp
// ✅ Expression-bodied property
public string FullName => $"{First} {Last}";

// ✅ Block-bodied method
public void Save()
{
    _repository.Add(this);
    _repository.Commit();
}
```

---

## 8. C# — Pattern Matching

Prefer modern pattern matching syntax. All enforced as `suggestion`.
```csharp
// ✅ Pattern matching over cast
if (shape is Circle c) { ... }

// ✅ Pattern matching over null check
if (order is not null) { ... }

// ✅ Switch expression
var label = status switch
{
    Status.Active => "Active",
    Status.Closed => "Closed",
    _ => "Unknown"
};
```

---

## 9. C# — Null Checking

Prefer modern null-safe constructs. All enforced as `suggestion`.
```csharp
// ✅ Null-coalescing
string name = input ?? "default";

// ✅ Null-conditional
int? length = text?.Length;

// ✅ Null-conditional invocation
handler?.Invoke(this, args);

// ✅ Throw expression
_service = service ?? throw new ArgumentNullException(nameof(service));

// ✅ is null check
if (value is null) { ... }
```

---

## 10. C# — Modern Language Features

| Feature | Setting | Effect |
|---|---|---|
| Simple using | `csharp_prefer_simple_using_statement` | Prefers `using var x = …` |
| Range operator | `csharp_style_prefer_range_operator` | Prefers `arr[1..^1]` (silent) |
| Index operator | `csharp_style_prefer_index_operator` | Prefers `arr[^1]` (silent) |
| Target-typed new | `csharp_style_implicit_object_creation_when_type_is_apparent` | Prefers `new()` when type is known |
| File-scoped namespaces | `csharp_style_namespace_declarations` | **Required** (`warning`) |
| Primary constructors | `csharp_style_prefer_primary_constructors` | Prefers `class Foo(int x)` |
| Collection expressions | `dotnet_style_prefer_collection_expression` | Prefers `[1, 2, 3]` syntax |
```csharp
// ✅ File-scoped namespace
namespace MyApp.Services;

// ✅ Target-typed new
List<Order> orders = new();

// ✅ Simple using
using var stream = File.OpenRead(path);
```

---

## 11. C# — Modifier Preferences

- **Accessibility modifiers** are required on all non-interface members (`suggestion`).
- **Fields** should be marked `readonly` where possible (`suggestion`).
- **Local functions** should be `static` when they don't capture instance state (`suggestion`).

**Canonical modifier order:**
```
public · private · protected · internal · file · static · extern · new
· virtual · abstract · sealed · override · readonly · unsafe · required
· volatile · async
```

---

## 12. C# — Braces

Always use braces for control flow blocks, even single-line ones. Enforced as `warning`.
```csharp
// ✅ Always use braces
if (isValid)
{
    Process();
}

// ❌ Avoid — easy to introduce bugs when adding lines
if (isValid)
    Process();
```

---

## 13. C# — New Lines (Allman Style)

Opening braces always appear on their own line. This is the Microsoft C# default.
```csharp
// ✅ Allman style
if (condition)
{
    DoSomething();
}
else
{
    DoSomethingElse();
}

try
{
    Run();
}
catch (Exception ex)
{
    Log(ex);
}
finally
{
    Cleanup();
}
```

Object initializers and LINQ clauses also get one member/clause per line:
```csharp
var order = new Order
{
    Id = 1,
    Name = "Test",
};

var result = from o in orders
             where o.IsActive
             select o.Name;
```

---

## 14. C# — Indentation

| Setting | Value | Effect |
|---|---|---|
| `csharp_indent_case_contents` | `true` | Indents code inside `case` blocks |
| `csharp_indent_switch_labels` | `true` | Indents `case` labels inside `switch` |
| `csharp_indent_labels` | `one_less_than_current` | `goto` labels are one level left of current block |
| `csharp_indent_block_contents` | `true` | Indents code inside `{ }` |
| `csharp_indent_braces` | `false` | Braces themselves are not indented |

---

## 15. C# — Spacing

| Rule | Example |
|---|---|
| No space after cast | `(int)x` |
| Space after control-flow keywords | `if (`, `for (`, `while (` |
| No space inside parentheses | `(x + y)` not `( x + y )` |
| Space around binary operators | `a + b`, `x == y` |
| No space before `(` in method calls | `Foo(x)` not `Foo (x)` |
| No space inside method call parens | `Foo(x, y)` not `Foo( x, y )` |
| Space after commas | `Foo(a, b, c)` |
| No spaces around `.` | `order.Id` |
| Space after `;` in `for` | `for (int i = 0; i < n; i++)` |

---

## 16. C# — Wrapping & Layout

| Setting | Value | Effect |
|---|---|---|
| `csharp_preserve_single_line_blocks` | `true` | Allows `{ return x; }` on one line |
| `csharp_preserve_single_line_statements` | `false` | Each statement must be on its own line |
| `csharp_place_attribute_on_same_line` | `never` | Attributes always on their own line above the member |
| `csharp_wrap_parameters_style` | `chop_if_long` | Wraps parameter lists only when they exceed the line limit |
| `csharp_max_array_initializer_elements_on_line` | `3` | Max 3 elements per line in collection initializers |
| `csharp_place_closing_parenthesis_on_new_line` | `false` | Closing `)` stays on the same line as the last argument |
| `csharp_keep_existing_arrangement` | `true` | Respects manual formatting instead of always reformatting |

---

## 17. Naming Conventions

| Symbol | Convention | Example | Severity |
|---|---|---|---|
| Interfaces | `I` + PascalCase | `IDisposable`, `IOrderService` | `error` |
| Type parameters | `T` + PascalCase | `TResult`, `TKey` | `warning` |
| Classes / structs / enums / delegates | PascalCase | `CustomerOrder`, `Status` | `warning` |
| Constants | PascalCase | `MaxRetries`, `DefaultTimeout` | `warning` |
| Non-private static fields | PascalCase | `public static MaxSize` | `suggestion` |
| Non-private readonly fields | PascalCase | `public readonly Version` | `suggestion` |
| Private static fields | `s_` + camelCase | `s_instance`, `s_cache` | `suggestion` |
| Private instance fields | `_` + camelCase | `_logger`, `_repository` | `suggestion` |
| Methods / properties / events | PascalCase | `GetOrder`, `OrderPlaced` | `warning` |
| Parameters | camelCase | `customerId`, `orderDate` | `suggestion` |
| Local variables / local functions | camelCase | `totalCount`, `computeSum` | `suggestion` |
| Async methods | PascalCase + `Async` suffix | `GetOrderAsync`, `SaveAsync` | `warning` |

> **Note:** `CA1707` warns on underscores in member names. This conflicts with the `_camelCase`
> field convention intentionally — the field naming rule takes precedence. The `CA1707`
> warning is suppressed in test projects to allow underscore-separated test method names
> like `GetOrder_WhenMissing_Returns404`.

---

## 18. Diagnostic Severities

### Compiler (CS)

| Code | Description | Severity |
|---|---|---|
| `CS0168` | Variable declared but never used | `suggestion` |
| `CS8602` | Possible null dereference | `suggestion` |
| `CS8604` | Possible null passed as non-nullable argument | `suggestion` |
| `CS8618` | Non-nullable field/property not initialized in constructor | `suggestion` |
| `CS8766` | Return type nullability mismatch with interface | `suggestion` |

### Code Quality (CA)

| Code | Description | Severity |
|---|---|---|
| `CA1050` | Declare types in namespaces | `none` |
| `CA1001` | Types owning disposable fields should be disposable | `none` |
| `CA1024` | Consider using a property instead of a method | `none` |
| `CA1031` | Do not catch general exception types | `none` |
| `CA1054` | URI parameters should not be strings | `none` |
| `CA1303` | Do not pass literals as localized parameters | `none` |
| `CA1305` | Specify IFormatProvider | `none` |
| `CA1307` | Specify StringComparison for clarity | `none` |
| `CA1310` | Specify StringComparison for correctness | `none` |
| `CA1416` | Platform compatibility (Windows-only app) | `none` |
| `CA1707` | Remove underscores from member names | `warning` |
| `CA1716` | Identifiers should not match keywords | `suggestion` |
| `CA1805` | Do not initialize fields unnecessarily | `none` |
| `CA1810` | Initialize static fields inline | `none` |
| `CA1816` | Dispose should call GC.SuppressFinalize | `none` |
| `CA1822` | Member can be marked static | `suggestion` |
| `CA1852` | Type can be sealed | `none` |
| `CA2007` | Do not directly await Task (app code) | `none` |
| `CA2234` | Pass System.Uri instead of string | `none` |

### IDE / Style

| Code | Description | Severity |
|---|---|---|
| `IDE0003` | Remove redundant `this.` qualification | `suggestion` |
| `IDE0005` | Remove unnecessary using directives | `warning` |
| `IDE0011` | Add braces (handled by `csharp_prefer_braces`) | `none` |
| `IDE0040` | Add missing accessibility modifiers | `suggestion` |
| `IDE0044` | Make field readonly | `suggestion` |
| `IDE0055` | Fix formatting (Rider handles formatting) | `none` |
| `IDE0060` | Remove unused parameter | `suggestion` |
| `IDE0161` | Convert to file-scoped namespace | `warning` |
| `IDE1006` | Naming rule violation (per-rule severity used instead) | `none` |

### Other

| Setting | Value | Effect |
|---|---|---|
| `dotnet_code_quality_unused_parameters` | `all:warning` | Warns on any unused method parameter |
| `resharper_check_namespace_highlight` | `none` | Suppresses namespace/folder mismatch highlights in Rider |

---

## 19. File-Specific Overrides

### UI Dialog files — `**/UI/**/*Dialog.cs`

These files are often partially or fully generated by a UI designer. Naming and
nullability warnings are suppressed to avoid noise in auto-generated code.

| Setting | Value |
|---|---|
| `generated_code` | `true` |
| `CS8669` | `none` — nullability annotations in generated code |
| `IDE1006` | `none` — generated names may not follow conventions |

### Test projects — `*Tests/**/*.cs`

Test method names conventionally use underscores to separate sections
(e.g. `GetOrder_WhenMissing_Returns404`). The following rules are relaxed:

| Setting | Value | Reason |
|---|---|---|
| `async_methods_should_end_with_async` | `none` | Tests don't always need the `Async` suffix |
| `CA1707` | `none` | Underscores are valid and encouraged in test method names |
