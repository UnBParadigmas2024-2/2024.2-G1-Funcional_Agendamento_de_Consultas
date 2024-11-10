**Detalhes de Instalação**: Aconselha-se utilizar o sistema operacional LINUX, para maior praticidade de instalação e uso do sistema.

- Passo 1. Instale o Haskell em sua máquina, com os seguintes passos:

```bash
sudo apt update
```

```bash
sudo apt install haskell-platform
```

- Passo 2. Instale o Compilador GHC

```bash
 curl -sSL https://get.haskellstack.org/ | sh
```

- Passo 3. Instalar o cabal

```bash
sudo apt install cabal-install
```

- Passo 4. Verifique se eleS foram instalados corretamente:

```bash
 ghc --version
```

```bash
 stack --version
```
