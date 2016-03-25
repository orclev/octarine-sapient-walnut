# octarine-sapient-walnut

## How To Run It

- Create new self-signed certs (for development)
- Place crt and key file in certs folder, name should match the VIRTUAL_HOST value in docker-compose.yml
- If necessary setup docker-machine
- $ stack setup
- $ stack build
- $ stack image container
- $ docker-compose up