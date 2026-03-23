# Coding Style

## Immutability

Prefer immutable patterns where possible:
- Create new objects instead of modifying existing ones
- Immutable data prevents hidden side effects
- Enables safe concurrency and easier debugging

## File Organization

Many small files over few large files:
- High cohesion, low coupling
- 200-400 lines typical, 800 max
- Extract utilities from large modules
- Organize by feature/domain, not by type

## Error Handling

Handle errors comprehensively:
- Handle errors explicitly at every level
- Provide user-friendly error messages
- Log detailed error context
- Never silently swallow errors

## Input Validation

Validate at system boundaries:
- Validate all input before processing
- Use schema-based validation where available
- Fail fast with clear error messages
- Never trust external data

## Comment Standards

- All comments in English
- Put comments on separate lines above code, not inline
- No alignment spaces for visual formatting
- Use TODO/FIXME format: `TODO(author): description`

## Line Length

- Code: 80 characters (soft limit)
- Comments: 79 characters (hard limit)
- URLs exempt

## Code Quality Checklist

Before marking work complete:
- [ ] Code is readable and well-named
- [ ] Functions are small (<50 lines)
- [ ] Files are focused (<800 lines)
- [ ] No deep nesting (>4 levels)
- [ ] Proper error handling
- [ ] No hardcoded values
