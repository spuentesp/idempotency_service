Idempotency Service

An OTP (Open Telecom Platform) application designed to normalize and handle non-idempotent data by applying configurable rules to ensure idempotency. This service reads a set of user-defined rules from a JSON file and uses pattern matching to generate consistent idempotency keys.

---

Features
- Configurable idempotency rules defined in JSON.
- Pattern matching for flexible normalization.
- OTP-compliant structure for scalability and maintainability.

---

Installation

Prerequisites
- Erlang/OTP 27 or later
- rebar3 (version 3.24.0 or later)
- OpenSSL (version 3.0 or later)

Clone the Repository
git clone <repository_url>
cd idempotency_service

Environment Configuration
If you encounter issues with SSL certificates during dependency installation, ensure the following:

export SSL_CERT_FILE=/opt/homebrew/Cellar/openssl@3/3.4.0/etc/cacert.pem

Build the Project
Compile the application using rebar3:
rebar3 compile

---

Usage

Loading Configuration
Place your idempotency rules in a JSON file, for example, idempotency_rules.json, located in the root directory. An example of the JSON structure:

{
  "rules": [
    {
      "name": "default",
      "ignore_fields": ["timestamp"],
      "key_fields": "all"
    },
    {
      "name": "payment",
      "ignore_fields": ["timestamp", "request_id"],
      "key_fields": ["user_id", "amount"]
    }
  ]
}

Running in Shell
Launch the interactive shell:
rebar3 shell

Load the configuration file:
config_loader:load_rules("idempotency_rules.json").

---

Project Structure

idempotency_service/
├── src/                       # Source files
│   ├── config_loader.erl      # Module for loading and parsing configuration
│   ├── idempotency_service.erl # Main application module
├── test/                      # Test files (TBD)
├── idempotency_rules.json     # Example rules (user-defined)
├── _build/                    # Build artifacts (generated)
├── rebar.config               # Rebar3 configuration
└── README.md                  # Project documentation

---

Roadmap

Done
- Module: config_loader
  - Load rules from a JSON file.
  - Parse JSON into maps for pattern matching.

- Basic Structure:
  - src/ folder with OTP-compliant modules.
  - Rebar3 build system initialized.

- SSL Configuration Fixes:
  - Instructions for setting SSL_CERT_FILE to resolve dependency installation issues.

To Do
1. Develop Core Normalization Logic
   - Apply rules to incoming data to generate idempotency keys.

2. Error Handling:
   - Graceful handling for invalid JSON or missing keys.

3. Testing Suite:
   - Add unit tests for config_loader and normalization logic.

4. CLI Support:
   - Allow users to test configurations and transformations via command line.

5. Docker Integration:
   - Create a Dockerfile for containerized deployment.

6. Documentation:
   - Expand this README with examples and edge cases.

7. CI/CD Pipeline:
   - Automate tests and builds with GitHub Actions or similar.

---

Contributing
Contributions are welcome! Please fork the repository and submit a pull request.

---

License
This project is licensed under the MIT License. See the LICENSE file for details.

