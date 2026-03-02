# PyMO2NScr

一个用 Haskell 编写的编译器，用于将 PyMO 视觉小说游戏转换为 NScripter / ONScripter 格式。

## TODO

- [x] 输出 defines
- [x] 输出 header
- [ ] 完善所有 Command Handler
- [ ] 输出 nscript.dat
- [ ] 构建 arc.nsa
- [ ] 复制 icon.png
- [ ] 复制 default.txt

## 项目简介

PyMO2NScr 是一个跨平台的转换工具，能够将使用 PyMO 引擎开发的游戏转换为兼容 NScripter/ONScripter 引擎的脚本。该项目旨在帮助游戏开发者将现有的 PyMO 游戏迁移到更广泛支持的 NScripter 平台。

## 功能特性

- **完整的 PyMO 脚本转换**：支持 PyMO 的主要命令和语法
- **多编码支持**：支持 GBK、UTF-8 和 ShiftJIS 编码输出
- **变量映射**：自动将 PyMO 变量映射到 NScripter 变量系统
- **标签处理**：正确处理 PyMO 脚本中的标签和跳转
- **模板系统**：使用模板文件生成格式化的 NScr 输出
- **跨平台**：支持 Windows、Linux 和 macOS

## 系统要求

- **Haskell 工具链**：Stack (推荐) 或 Cabal
- **Git**：用于克隆项目和子模块
- **Python 3**：（可选）用于运行测试脚本

## 安装与构建

### 1. 克隆项目

```bash
git clone --recursive https://github.com/PyMO-Ecosphere/PyMO2NScr.git
cd PyMO2NScr
```

如果没有使用 `--recursive` 参数，需要手动初始化子模块：

```bash
git submodule update --init --recursive
```

### 2. 构建项目

然后构建项目：

```bash
stack build
```

### 3. 安装可执行文件

```bash
stack install
```

安装后，`PyMO2NScr` 可执行文件将添加到 PATH 中。

## 使用方法

### 基本用法

```bash
PyMO2NScr <pymo-dir> [output-dir] [--encoding <gbk/en/sjis>]
```

### 参数说明

- `<pymo-dir>`：PyMO 游戏目录（必需）
- `[output-dir]`：输出目录（可选，默认为当前目录）
- `[--encoding <gbk/en/sjis>]`：输出编码（可选，默认为 gbk）

### 编码选项

- `gbk`：中文本地化 NScripter（MiNE, ONScripter-Jh）
- `en`：ONScripter-EN（UTF-8 编码）
- `sjis`：原始 ONScripter/NScripter（ShiftJIS 编码）

### 示例

```bash
# 转换 PyMO 游戏到当前目录，使用 GBK 编码
PyMO2NScr ./my-pymo-game/

# 转换到指定目录，使用 UTF-8 编码
PyMO2NScr ./my-pymo-game/ ./output/ --encoding en

# 转换到指定目录，使用 ShiftJIS 编码
PyMO2NScr ./my-pymo-game/ ./output/ --encoding sjis
```

## 项目结构

```
PyMO2NScr/
├── src/
│   ├── Main.hs              # 命令行接口和编码处理
│   ├── Compiler.hs          # 编译器核心逻辑和状态管理
│   ├── ScriptCompiler.hs    # 脚本级编译逻辑
│   ├── CommandCompiler.hs   # 命令转换实现
│   └── nscr-template.txt    # NScr 输出模板
├── test/                    # 测试数据
├── PyMO-Parser/            # PyMO 文件解析库（子模块）
├── Nscripter-API/          # NScripter API 参考（子模块）
├── stack.yaml              # Stack 配置文件
├── PyMO2NScr.cabal         # Cabal 配置文件
└── LICENSE                 # AGPL-3.0 许可证
```

### 主要模块

1. **Main.hs** (`src/Main.hs:1`)
   - 处理命令行参数
   - 管理文本编码转换
   - 调用编译器主流程

2. **Compiler.hs** (`src/Compiler.hs:1`)
   - 编译器状态管理（`CompilerState`）
   - 模板系统集成
   - 输出文件生成

3. **ScriptCompiler.hs** (`src/ScriptCompiler.hs:1`)
   - 脚本分析和标签处理
   - 命令分发到 CommandCompiler

4. **CommandCompiler.hs** (`src/CommandCompiler.hs:1`)
   - 实现 PyMO 命令到 NScripter 的转换
   - 支持 `goto`、`change`、`call` 等命令

### 编译器状态

编译器使用 `RWST` monad 管理状态，包括：
- 本地变量和全局变量映射
- 已加载脚本缓存
- 已编译脚本记录

### 模板系统

输出通过填充 `src/nscr-template.txt` 中的占位符生成：
```
&&header

*define
&&defines
game

*start
&&body
```

## 开发说明

### 开发环境设置

1. 安装 Haskell 工具链（推荐使用 [GHCup](https://www.haskell.org/ghcup/)）
2. 安装 Stack：`ghcup install stack`
3. 克隆项目并初始化子模块
4. 构建项目：`stack build`

### 运行测试

项目包含示例测试数据：

```bash
stack exec PyMO2NScr -- test/ output/
```

测试数据位于 `test/` 目录，包含：
- `gameconfig.txt`：PyMO 游戏配置
- `script/`：示例脚本文件
- 完整的资源目录（bg, bgm, chara, se, system, voice）

### 添加新命令支持

要添加新的 PyMO 命令支持：

1. 在 `CommandCompiler.hs` 中添加新的处理函数
2. 在命令分发器中注册新函数
3. 编写相应的 NScripter 输出逻辑

### 调试

可以使用 Stack 的调试功能：

```bash
stack exec -- ghci src/Compiler.hs
```

## 贡献指南

欢迎贡献代码、报告问题或提出改进建议！

### 贡献流程

1. Fork 本仓库
2. 创建功能分支：`git checkout -b feature/amazing-feature`
3. 提交更改：`git commit -m 'Add amazing feature'`
4. 推送到分支：`git push origin feature/amazing-feature`
5. 创建 Pull Request

### 代码规范

- 遵循 Haskell 社区编码规范
- 使用 `-Wall` 编译选项确保代码质量
- 为新增功能添加注释说明
- 保持与现有代码风格一致

### 提交信息格式

使用清晰的提交信息，描述更改的内容和原因：

```
类型: 简要描述

详细描述更改内容，包括：
- 修改的文件
- 实现的功能
- 解决的问题

相关 issue: #123
```

## 许可证

本项目采用 GNU Affero General Public License v3.0 (AGPL-3.0) 许可证。详情请参阅 [LICENSE](LICENSE) 文件。

## 致谢

- [PyMO](https://github.com/zyr17/Pymo)：原始的 Python 视觉小说引擎
- NScripter/ONScripter 社区：提供了强大的视觉小说引擎
- 所有贡献者和用户

## 联系方式

- 项目主页：https://github.com/PyMO-Ecosphere/PyMO2NScr
- 问题反馈：https://github.com/PyMO-Ecosphere/PyMO2NScr/issues
- 作者：Seng-Jik (xc1998@foxmail.com)
