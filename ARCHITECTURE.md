# Relian Architecture & Guidelines

## Overview

Relian is a Legacy Refactoring Substrate designed to modernize legacy systems.

## Architecture Stack

- **Pattern**: Microservices
- **Database**: PostgreSQL (Relational) + Neo4j (Graph)
- **API**: REST + GraphQL
- **Frontend**: React + TypeScript
- **Blockchain**: Solana (Anchor framework)

## Code Style

### Python

- **Formatter**: Black
- **Linter**: Pylint + Mypy
- **Max Line Length**: 100
- **Docstring Style**: Google

### TypeScript

- **Formatter**: Prettier
- **Linter**: ESLint
- **Style Guide**: Airbnb

## Testing

- **Frameworks**: pytest (Python), jest (TypeScript)
- **Coverage Target**: 80%
- **Benchmark Target**: Pass all defined metrics

## Dependencies

- **Python**: >=3.11
- **Node**: >=18.0.0
- **Rust**: >=1.75.0
