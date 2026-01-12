# Nyxt-Proton

A Proton Pass password manager integration for the [Nyxt browser](https://nyxt.atlas.engineer/).

## Features

- **Fill credentials** automatically on web pages
- **Search logins** by title or username
- **TOTP support** for two-factor authentication
- **Extensible architecture** - easy to add other password managers
- **Security-focused** - minimizes secret residency in memory
- **Future-ready** - designed for save/update, passkeys, and more

## Architecture

Nyxt-Proton is built with a two-layer architecture that keeps the door open for future features:

### Layer 1: Password Manager Interface (PMI)

A generic Common Lisp protocol (in `src/protocol.lisp`) that Nyxt code calls, independent of any specific password manager. This defines:

- **Core entities**: `pm-item`, `pm-credential`, `pm-otp`, `pm-passkey`
- **Core operations**: `pm-search-items`, `pm-get-item`, `pm-fill`, `pm-generate-totp`, `pm-save-login`, `pm-update-login`
- **Capability discovery**: `pm-capabilities` to check what operations a backend supports

### Layer 2: Backend Adapters

Pluggable backends that implement the PMI protocol. Currently includes:

- **Proton Pass CLI adapter** (`src/adapters/proton-pass-cli.lisp`) - uses the Proton Pass command-line tool

The architecture makes it easy to add other backends in the future (Bitwarden CLI, KeePassXC, direct Proton client library, etc.) without changing Nyxt integration code.

### Additional Modules

- **Fill module** (`src/fill.lisp`) - DOM field discovery and credential filling with proper event handling
- **Mode** (`src/mode.lisp`) - Nyxt integration with commands and UI
- **Main** (`src/main.lisp`) - Public API and initialization

## Installation

### Prerequisites

1. **Nyxt browser** - Install from https://nyxt.atlas.engineer/
2. **Proton Pass CLI** - Install and configure the Proton Pass command-line tool
   - You must be logged in to Proton Pass via the CLI
   - Test with: `proton-pass item list`

### Install via Quicklisp (when available)

```lisp
(ql:quickload :nyxt-proton)
```

### Install from source

1. Clone this repository:
   ```bash
   git clone https://github.com/your-username/nyxt-proton.git
   cd nyxt-proton
   ```

2. Create a symlink in your Quicklisp local-projects directory:
   ```bash
   ln -s $(pwd) ~/quicklisp/local-projects/nyxt-proton
   ```

3. Load in Common Lisp:
   ```lisp
   (ql:quickload :nyxt-proton)
   ```

## Configuration

Add to your Nyxt configuration file (`~/.config/nyxt/config.lisp`):

### Basic Setup

```lisp
;; Load the extension
(ql:quickload :nyxt-proton)

;; Initialize Proton Pass backend
(nyxt-proton:initialize-proton-pass)

;; Optional: Enable password-manager-mode for all buffers
(define-configuration buffer
  ((default-modes (append '(nyxt-proton/mode:password-manager-mode) %slot-default%))))
```

### Custom CLI Path

If your Proton Pass CLI is not in the system PATH:

```lisp
(nyxt-proton:initialize-proton-pass :cli-path "/custom/path/to/proton-pass")
```

### Keyboard Shortcuts (Recommended)

```lisp
(define-configuration buffer
  ((override-map
    (let ((map (make-keymap "password-manager")))
      (define-key map
        "C-c p f" 'nyxt-proton/mode:pm-fill-for-page
        "C-c p s" 'nyxt-proton/mode:pm-search
        "C-c p u" 'nyxt-proton/mode:pm-copy-username
        "C-c p p" 'nyxt-proton/mode:pm-copy-password
        "C-c p t" 'nyxt-proton/mode:pm-show-totp)
      map))))
```

## Usage

### Fill Credentials for Current Page

1. Navigate to a login page
2. Run: `pm-fill-for-page` (or press `C-c p f`)
3. If multiple matches are found, select the desired login
4. Credentials are automatically filled

### Search for a Login

1. Run: `pm-search` (or press `C-c p s`)
2. Enter search query (matches title or username)
3. Select a login from results
4. Credentials are filled on current page

### Copy Username/Password

After filling credentials:
- Run `pm-copy-username` (or `C-c p u`) to copy username to clipboard
- Run `pm-copy-password` (or `C-c p p`) to copy password to clipboard

### Generate TOTP Code

After filling credentials for an item with TOTP configured:
1. Run: `pm-show-totp` (or press `C-c p t`)
2. The TOTP code is displayed and copied to clipboard
3. Shows expiry countdown

## Available Commands

- `pm-fill-for-page` - Fill credentials for the current page
- `pm-search` - Search for a login by title or username
- `pm-copy-username` - Copy username to clipboard (of last filled item)
- `pm-copy-password` - Copy password to clipboard (of last filled item)
- `pm-show-totp` - Generate and copy TOTP code (for last filled item)

## Current Capabilities

### Implemented (v0.0.1)

- ✅ Fill username and password on web pages
- ✅ Search logins by origin (URL matching)
- ✅ Search logins by title/username
- ✅ TOTP code generation
- ✅ Clipboard operations
- ✅ Multiple vault support (via Proton Pass CLI)
- ✅ Smart field detection (autocomplete attributes, field types, etc.)
- ✅ Event dispatching for JavaScript framework compatibility

### Planned (Future Versions)

- 🔜 Save new credentials (when supported by Proton Pass CLI)
- 🔜 Update existing credentials (when supported by Proton Pass CLI)
- 🔜 Auto-detect form submissions and prompt to save
- 🔜 Passkey support (when browser engine allows)
- 🔜 Additional backend adapters (Bitwarden, KeePassXC, etc.)
- 🔜 Automatic clipboard clearing after timeout
- 🔜 Per-site fill strategy customization

## Architecture Deep Dive

### Why This Design?

The two-layer architecture provides several benefits:

1. **Extensibility**: Easy to add new password managers without changing Nyxt code
2. **Future-proof**: Designed for features not yet implemented (save/update, passkeys)
3. **Capability discovery**: Backends declare what they support, UI adapts accordingly
4. **Security**: Secrets are fetched only when needed, not cached
5. **Separation of concerns**: Protocol, backend, filling, and UI are independent modules

### How Filling Works

1. **Field Discovery**: JavaScript scans the page for login fields using:
   - Input types (password, email, text)
   - Autocomplete attributes (username, current-password, email)
   - Common name/id patterns
   - Proximity to password fields

2. **Field Scoring**: Username candidates are scored based on:
   - Autocomplete attributes (highest priority)
   - Field type (email > text)
   - Name/ID patterns
   - Visibility
   - Position in DOM

3. **Filling with Events**: Values are set using:
   - Native property setters (bypasses some restrictions)
   - Input/change/blur event dispatching
   - Focus/blur cycle for validation triggers

   This ensures JavaScript frameworks (React, Vue, Angular) detect the changes.

### Security Considerations

- **Lazy secret loading**: Metadata is loaded first, secrets only when needed
- **No caching**: Passwords are not stored in memory after filling
- **Process boundary**: Secrets flow CLI → Lisp → DOM and are then discarded
- **Optional clipboard clearing**: Planned feature to auto-clear after timeout
- **No logging**: Debug logging is disabled by default to avoid exposing secrets

## Proton Pass CLI Requirements

This extension relies on the Proton Pass CLI. Required commands:

- `proton-pass item list --format json` - List items
- `proton-pass item get <id> --format json` - Get full item details
- `proton-pass item totp <id>` - Generate TOTP code (if supported)
- `proton-pass vault list --format json` - List vaults (optional)

**Note**: The exact CLI API may vary depending on Proton Pass CLI version. This implementation is based on expected functionality. If the CLI API differs, the adapter may need adjustments.

## Development

### Project Structure

```
nyxt-proton/
├── nyxt-proton.asd              # ASDF system definition
├── README.markdown              # This file
├── src/
│   ├── protocol.lisp            # PMI protocol definition
│   ├── adapters/
│   │   └── proton-pass-cli.lisp # Proton Pass CLI adapter
│   ├── fill.lisp                # DOM field discovery and filling
│   ├── mode.lisp                # Nyxt mode and commands
│   └── main.lisp                # Public API and initialization
└── tests/
    ├── protocol-test.lisp       # Protocol tests
    ├── adapter-test.lisp        # Adapter tests
    ├── integration-test.lisp    # Integration tests (optional)
    └── main.lisp                # Test suite entry point
```

### Adding a New Backend

To add support for another password manager:

1. Create a new file in `src/adapters/` (e.g., `bitwarden-cli.lisp`)
2. Define a class that inherits from `pm-backend`
3. Implement the generic functions from `src/protocol.lisp`:
   - `pm-capabilities`
   - `pm-search-items`
   - `pm-get-item`
   - `pm-fill` (or delegate to fill module)
   - Optionally: `pm-generate-totp`, `pm-save-login`, `pm-update-login`, etc.
4. Export a constructor function (e.g., `make-bitwarden-backend`)
5. Update `src/main.lisp` to export the new backend

### Running Tests

Nyxt-Proton includes a comprehensive test suite covering protocol, adapter, and integration testing.

#### Quick Start

```bash
# Run all tests (excluding integration tests)
sbcl --eval '(ql:quickload :nyxt-proton/tests)' \
     --eval '(asdf:test-system :nyxt-proton)' \
     --quit
```

Or from the REPL:

```lisp
(ql:quickload :nyxt-proton/tests)
(asdf:test-system :nyxt-proton)
```

#### Test Organization

The test suite is organized into three main files:

1. **`tests/protocol-test.lisp`** - Protocol and data structure tests
   - PM-item, pm-credential, pm-otp creation
   - URL normalization
   - Error conditions
   - Mock backend implementation

2. **`tests/adapter-test.lisp`** - Proton Pass CLI adapter tests
   - Backend creation and configuration
   - Capabilities reporting
   - URL origin matching
   - JSON and item parsing
   - Error handling

3. **`tests/integration-test.lisp`** - Integration tests (optional)
   - Real Proton Pass CLI interaction
   - Search and retrieval operations
   - Performance testing

#### Running Integration Tests

Integration tests interact with your actual Proton Pass vault and require:

1. Proton Pass CLI installed and configured
2. Active Proton Pass session (logged in)
3. Environment variable set

To run with integration tests:

```bash
# Set environment variable
export NYXT_PROTON_RUN_INTEGRATION_TESTS=1

# Run tests
sbcl --eval '(ql:quickload :nyxt-proton/tests)' \
     --eval '(asdf:test-system :nyxt-proton)' \
     --quit
```

**Note**: Integration tests are SKIPPED by default to avoid requiring external dependencies.

#### Test Coverage Summary

To see what's tested:

```lisp
(ql:quickload :nyxt-proton/tests)
(in-package :nyxt-proton/tests/main)
(test-status)
```

#### Writing New Tests

When adding new features, add corresponding tests:

1. **Protocol changes**: Add tests to `tests/protocol-test.lisp`
2. **Adapter logic**: Add tests to `tests/adapter-test.lisp`
3. **Integration features**: Add tests to `tests/integration-test.lisp`

Example test:

```lisp
(deftest test-new-feature
  (testing "description of what's being tested"
    (ok (= (my-function) expected-result))))
```

#### Continuous Integration

For CI environments where Proton Pass CLI is not available:

```bash
# Run without integration tests (default)
make test

# Or explicitly disable
unset NYXT_PROTON_RUN_INTEGRATION_TESTS
sbcl --eval '(asdf:test-system :nyxt-proton)'
```

The test suite will automatically skip integration tests and report success for unit tests only.

## Troubleshooting

### "No password manager backend configured"

Make sure you've called `(nyxt-proton:initialize-proton-pass)` in your config.

### "CLI error" or authentication issues

Ensure you're logged in to Proton Pass:
```bash
proton-pass auth login
```

### Fields not filling correctly

Some sites use unusual field structures or JavaScript frameworks with custom validation. Try:
1. Filling manually once to "teach" the site
2. Checking the browser console for JavaScript errors
3. Opening an issue with the problematic site URL

### TOTP not working

- Verify TOTP is configured for the item in Proton Pass
- Check if the CLI supports `proton-pass item totp <id>` command
- Some CLI versions may use different commands or formats

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

## Author

* Peter Selby (pselby@gmail.com)

## Copyright

Copyright (c) 2026 Peter Selby (pselby@gmail.com)

## License

Licensed under the LLGPL License.

## Changelog

### v0.0.1 (2026-01-12)

- Initial release
- Fill credentials on web pages
- Search logins by origin and title
- TOTP support
- Proton Pass CLI adapter
- Extensible architecture for future features
