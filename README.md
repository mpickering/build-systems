# Haskell Build System Framework

This repository contains a framework for implementing and testing build systems in Haskell. The codebase demonstrates different approaches to implementing build systems with dependency tracking, rule-based execution, and caching.

## Project Structure

The project is organized into three main directories:

- **Abstract/** - Contains abstract build rule examples and data structures
  - `Simple.hs` - Defines abstract module compilation and dependency rules

- **System/** - Contains build system implementations
  - `Shake.hs` - An implementation of a build system inspired by Shake, providing a monadic interface for rule definition and execution

- **Example/** - Contains concrete examples that combine the abstract rules with system implementations
  - `SimpleShake.hs` - Demonstrates using the Shake-like system to compile modules with dependencies
  - `Spreadsheet.hs` - Implements a spreadsheet calculation engine using the build system framework
