# Assembly Comments Feature

## Overview
The compiler now supports adding comments to generated x86_64 assembly code using the `Instr::Comment` variant.

## Usage

### Adding Comments in Code Generation
To add a comment to your generated assembly, use:
```rust
instrs.push(Instr::Comment("Your comment here".to_string()));
```

### Example
```rust
// In your compilation function:
instr_vec.push(Instr::Comment("Let bindings start".to_string()));
for (name, bind_expr) in bindings {
    instr_vec.push(Instr::Comment(format!("Binding: {}", name)));
    // ... rest of compilation logic
}
```

### Generated Assembly
Comments appear in the assembly with a semicolon prefix:
```asm
; Let bindings start
; Binding: x
mov rax, 10
; Binding: y
mov rax, 20
```

## Implementation Details

- **Enum Variant**: `Instr::Comment(String)` added to the `Instr` enum
- **String Output**: Comments are rendered as `; <text>` in assembly
- **JIT Compilation**: Comments are safely ignored during JIT execution (they generate no machine code)
- **No Performance Impact**: Comments are purely for readability and debugging

## Benefits

1. **Improved Readability**: Makes generated assembly easier to understand
2. **Debugging**: Helps trace back assembly to source constructs
3. **Documentation**: Self-documents what each section of code does
4. **No Runtime Cost**: Comments don't affect program execution or performance
