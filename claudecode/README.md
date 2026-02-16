# Claude Code 完整安装配置指南

本文档提供 Claude Code 的完整安装和配置指南，包括 GLM 模型配置、MCP 服务器、Plugins 和 Skills 的详细说明。

## 目录

- [前提条件](#前提条件)
- [快速安装](#快速安装)
- [配置概览](#配置概览)
- [Marketplace 与插件清单](#marketplace-与插件清单)
- [端到端产品研发插件配置](#端到端产品研发插件配置)
- [通用能力插件](#通用能力插件)
- [常用 Skills 参考](#常用-skills-参考)
- [常见问题](#常见问题)

---

## 前提条件

### 必需环境

- **Node.js**: 18 或更新版本
- **npm**: 随 Node.js 一起安装

### 安装 Node.js (macOS)

```bash
# 推荐：使用 nvm 安装 Node.js
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
source ~/.zshrc  # 或 source ~/.bashrc
nvm install --lts
nvm use --lts

# 或使用 Homebrew
brew install node
```

> **注意**: macOS 用户不推荐直接使用安装包安装 Node.js，后续可能会遇到权限问题。

---

## 快速安装

### 步骤一：安装 Claude Code

```bash
# 全局安装 Claude Code
npm install -g @anthropic-ai/claude-code

# 验证安装
claude --version
```

### 步骤二：使用智谱 Coding Tool Helper 配置

安装完成后，运行智谱 Coding Tool Helper 完成配置：

```bash
npx @z_ai/coding-helper
```

Coding Tool Helper 是一个交互式配置工具，可以自动完成：

1. **API 密钥管理** - 设置和更新 GLM Coding Plan API Key
2. **模型配置** - 自动配置 GLM 模型端点
3. **MCP 服务器** - 一键安装视觉、搜索、网页读取等 MCP 服务器
4. **插件市场** - 管理和安装 Claude Code 插件

运行后按照界面提示操作：`开始` → `编码工具` → `Claude Code` → 选择需要的功能

> **详细文档**: [Coding Tool Helper 使用指南](https://docs.bigmodel.cn/cn/coding-plan/extension/coding-tool-helper)

### 步骤三：验证配置

```bash
# 启动 Claude Code
claude

# 在 Claude Code 中验证
/status    # 查看当前模型和配置
/help      # 查看帮助
```

---

## 配置概览

### 配置文件位置

| 文件     | 路径                          | 用途                  |
|----------|-------------------------------|-----------------------|
| 全局设置 | `~/.claude/settings.json`     | 模型、环境变量、Plugins |
| 用户配置 | `~/.claude.json`              | MCP 服务器、项目配置   |
| 项目权限 | `.claude/settings.local.json` | 项目级权限设置        |

### GLM 模型配置

| 环境变量                         | 推荐值        | 说明                      |
|----------------------------------|---------------|---------------------------|
| `ANTHROPIC_DEFAULT_HAIKU_MODEL`  | `glm-4.5-air` | 轻量快速模型，用于简单任务 |
| `ANTHROPIC_DEFAULT_SONNET_MODEL` | `glm-5`       | 平衡模型，日常开发推荐     |
| `ANTHROPIC_DEFAULT_OPUS_MODEL`   | `glm-5`       | 最强模型，用于复杂任务     |

---

## Marketplace 与插件清单

当前配置的 Marketplace 及其插件如下：

### 1. claude-plugins-official (官方插件)

| Plugin            | 功能                                  |
|-------------------|---------------------------------------|
| `context7`        | 实时文档查询，获取最新库文档和代码示例 |
| `playwright`      | E2E 测试，浏览器自动化                 |
| `commit-commands` | Git 提交工作流                        |
| `firecrawl`       | 网页爬取和内容提取                    |

### 2. claude-code-workflows (工作流插件集)

这是最大的插件集合，覆盖完整的软件开发生命周期：

| Plugin                        | 功能                 |
|-------------------------------|----------------------|
| `agent-teams`                 | 多 Agent 协作开发    |
| `conductor`                   | 项目任务管理和追踪   |
| `backend-development`         | 后端开发最佳实践     |
| `frontend-mobile-development` | 前端和移动端开发     |
| `python-development`          | Python 开发支持      |
| `javascript-typescript`       | JS/TS 开发支持       |
| `systems-programming`         | Go/Rust/C++ 系统编程 |
| `tdd-workflows`               | 测试驱动开发工作流   |
| `unit-testing`                | 单元测试支持         |
| `code-review-ai`              | AI 代码审查          |
| `comprehensive-review`        | 全面代码质量审查     |
| `security-scanning`           | 安全漏洞扫描         |
| `cicd-automation`             | CI/CD 自动化         |
| `cloud-infrastructure`        | 云基础设施管理       |
| `kubernetes-operations`       | Kubernetes 运维      |
| `database-design`             | 数据库设计           |
| `api-scaffolding`             | API 脚手架生成       |
| `ui-design`                   | UI 设计和组件开发    |
| `documentation-generation`    | 文档生成             |
| `deployment-strategies`       | 部署策略             |
| `performance-testing-review`  | 性能测试             |
| `llm-application-dev`         | LLM 应用开发         |
| `machine-learning-ops`        | MLOps 支持           |
| `startup-business-analyst`    | 创业商业分析         |
| `seo-analysis-monitoring`     | SEO 分析监控         |
| `seo-content-creation`        | SEO 内容创作         |
| `content-marketing`           | 内容营销             |
| `debugging-toolkit`           | 调试工具集           |
| `error-diagnostics`           | 错误诊断             |
| `shell-scripting`             | Shell 脚本编写       |

### 3. thedotmack (社区插件)

| Plugin       | 功能                        |
|--------------|-----------------------------|
| `claude-mem` | 持久化记忆，跨会话保存上下文 |

### 4. zai-coding-plugins (智谱插件)

| Plugin           | 功能              |
|------------------|-------------------|
| `glm-plan-usage` | GLM Plan 用量查询 |

### 5. everything-claude-code (全能插件)

| Plugin                   | 功能                                     |
|--------------------------|------------------------------------------|
| `everything-claude-code` | 代码审查、TDD、安全扫描、E2E 测试等全能插件 |

### 6. marketingskills (营销插件)

| Plugin             | 功能                     |
|--------------------|--------------------------|
| `marketing-skills` | 营销策略、定价、转化优化等 |

### MCP 服务器

| 服务器             | 类型  | 功能                                 |
|--------------------|-------|--------------------------------------|
| `zai-mcp-server`   | stdio | 图像分析、视频分析、UI 转代码、错误诊断 |
| `web-search-prime` | http  | 网页搜索，获取最新信息                |
| `web-reader`       | http  | 网页内容读取和解析                   |
| `tavily`           | http  | 高级网页搜索和研究                   |

---

## 端到端产品研发插件配置

从产品构思到上线运营的完整链路，每个阶段推荐配置的插件：

### 阶段一：Brainstorm (创意构思)

**目标**: 市场调研、竞品分析、商业可行性评估

| Plugin                     | 主要 Skills                                    | 用途                  |
|----------------------------|------------------------------------------------|-----------------------|
| `startup-business-analyst` | `/market-opportunity`, `/business-case`        | 市场机会分析、商业案例 |
| `marketing-skills`         | `/competitor-alternatives`, `/marketing-ideas` | 竞品分析、营销创意     |
| `business-analytics`       | `/data-storytelling`                           | 数据驱动的商业洞察    |
| `content-marketing`        | `/search-specialist`                           | 市场调研搜索          |

**推荐 MCP**: `tavily` (深度研究), `web-search-prime` (快速搜索)

---

### 阶段二：Planning & Design (规划与设计)

**目标**: 产品设计、技术架构设计、数据库建模

#### 产品设计

| Plugin      | 主要 Skills                                 | 用途                |
|-------------|---------------------------------------------|---------------------|
| `ui-design` | `/design-system-setup`, `/create-component` | 设计系统、组件设计   |
| `ui-design` | `/accessibility-audit`, `/design-review`    | 无障碍审计、设计审查 |

#### 技术架构

| Plugin                | 主要 Skills                                         | 用途         |
|-----------------------|-----------------------------------------------------|--------------|
| `c4-architecture`     | `/c4-context`, `/c4-container`, `/c4-component`     | C4 架构文档  |
| `backend-development` | `/architecture-patterns`, `/microservices-patterns` | 后端架构设计 |
| `api-scaffolding`     | `/backend-architect`                                | API 架构设计 |
| `llm-application-dev` | `/langchain-architecture`                           | LLM 应用架构 |

#### 数据库设计

| Plugin             | 主要 Skills                       | 用途                |
|--------------------|-----------------------------------|---------------------|
| `database-design`  | `/database-architect`, `/sql-pro` | 数据库设计、SQL 优化 |
| `data-engineering` | `/data-engineer`                  | 数据管道设计        |

---

### 阶段三：Development (开发)

**目标**: 功能实现、代码编写

#### 前端开发

| Plugin                        | 主要 Skills                                   | 用途            |
|-------------------------------|-----------------------------------------------|-----------------|
| `frontend-mobile-development` | `/frontend-developer`, `/mobile-developer`    | 前端/移动端开发 |
| `javascript-typescript`       | `/javascript-pro`, `/typescript-pro`          | JS/TS 开发      |
| `ui-design`                   | `/responsive-design`, `/web-component-design` | 响应式设计      |

#### 后端开发

| Plugin                | 主要 Skills                                         | 用途         |
|-----------------------|-----------------------------------------------------|--------------|
| `backend-development` | `/backend-architect`, `/event-sourcing-architect`   | 后端架构实现 |
| `python-development`  | `/python-pro`, `/fastapi-pro`, `/django-pro`        | Python 开发  |
| `api-scaffolding`     | `/fastapi-pro`, `/django-pro`, `/graphql-architect` | API 脚手架   |
| `systems-programming` | `/golang-pro`, `/rust-pro`, `/cpp-pro`              | 系统编程     |

#### LLM/AI 应用开发

| Plugin                 | 主要 Skills                           | 用途        |
|------------------------|---------------------------------------|-------------|
| `llm-application-dev`  | `/ai-engineer`, `/rag-implementation` | AI 应用开发 |
| `machine-learning-ops` | `/ml-engineer`, `/mlops-engineer`     | ML 系统开发 |

#### 支付集成

| Plugin               | 主要 Skills                                  | 用途     |
|----------------------|----------------------------------------------|----------|
| `payment-processing` | `/stripe-integration`, `/paypal-integration` | 支付集成 |

---

### 阶段四：Testing (测试)

**目标**: 质量保证、测试覆盖

| Plugin                       | 主要 Skills                                    | 用途             |
|------------------------------|------------------------------------------------|------------------|
| `tdd-workflows`              | `/tdd`, `/tdd-red`, `/tdd-green`, `/tdd-cycle` | TDD 工作流       |
| `unit-testing`               | `/test-automator`                              | 单元测试         |
| `everything-claude-code`     | `/e2e-runner`                                  | E2E 测试         |
| `playwright`                 | `/firecrawl-cli` (E2E)                         | 浏览器自动化测试 |
| `python-development`         | `/python-testing-patterns`                     | Python 测试      |
| `javascript-typescript`      | `/javascript-testing-patterns`                 | JS 测试          |
| `systems-programming`        | `/golang-testing`, `/cpp-testing`              | 系统编程测试     |
| `performance-testing-review` | `/performance-engineer`                        | 性能测试         |

---

### 阶段五：Code Review & Security (代码审查与安全)

**目标**: 代码质量、安全合规

| Plugin                     | 主要 Skills                                          | 用途         |
|----------------------------|------------------------------------------------------|--------------|
| `comprehensive-review`     | `/full-review`, `/architect-review`                  | 全面代码审查 |
| `everything-claude-code`   | `/code-reviewer`, `/python-reviewer`, `/go-reviewer` | 专业代码审查 |
| `security-scanning`        | `/security-sast`, `/security-hardening`              | 安全扫描     |
| `backend-api-security`     | `/backend-security-coder`                            | 后端安全     |
| `frontend-mobile-security` | `/frontend-security-coder`, `/mobile-security-coder` | 前端安全     |
| `codebase-cleanup`         | `/refactor-cleaner`                                  | 代码清理     |

---

### 阶段六：CI/CD & Deployment (持续集成与部署)

**目标**: 自动化流水线、基础设施管理

#### CI/CD

| Plugin                  | 主要 Skills                                        | 用途         |
|-------------------------|----------------------------------------------------|--------------|
| `cicd-automation`       | `/github-actions-templates`, `/gitlab-ci-patterns` | CI/CD 流水线 |
| `deployment-strategies` | `/deployment-engineer`                             | 部署策略     |
| `deployment-validation` | `/cloud-architect`                                 | 部署验证     |

#### 云基础设施

| Plugin                 | 主要 Skills                                       | 用途            |
|------------------------|---------------------------------------------------|-----------------|
| `cloud-infrastructure` | `/cloud-architect`, `/terraform-specialist`       | 云架构          |
| `cloud-infrastructure` | `/kubernetes-architect`                           | Kubernetes      |
| `cloud-infrastructure` | `/hybrid-cloud-architect`, `/service-mesh-expert` | 混合云、服务网格 |

#### 容器编排

| Plugin                  | 主要 Skills                                        | 用途            |
|-------------------------|----------------------------------------------------|-----------------|
| `kubernetes-operations` | `/kubernetes-architect`, `/helm-chart-scaffolding` | K8s 运维        |
| `kubernetes-operations` | `/gitops-workflow`, `/k8s-security-policies`       | GitOps、安全策略 |

---

### 阶段七：Operations & Monitoring (运维与监控)

**目标**: 系统稳定性、性能优化

| Plugin                    | 主要 Skills                                        | 用途            |
|---------------------------|----------------------------------------------------|-----------------|
| `application-performance` | `/performance-engineer`, `/observability-engineer` | 性能优化        |
| `cicd-automation`         | `/devops-troubleshooter`                           | DevOps 故障排查 |
| `debugging-toolkit`       | `/debugger`, `/dx-optimizer`                       | 调试、开发者体验 |
| `error-debugging`         | `/debugger`, `/error-detective`                    | 错误诊断        |
| `backend-development`     | `/security-auditor`                                | 安全审计        |

---

### 阶段八：Marketing & Growth (营销与增长)

**目标**: 用户增长、SEO 优化、内容营销

#### SEO

| Plugin                       | 主要 Skills                                               | 用途     |
|------------------------------|-----------------------------------------------------------|----------|
| `seo-analysis-monitoring`    | `/seo-authority-builder`, `/seo-cannibalization-detector` | SEO 分析 |
| `seo-content-creation`       | `/seo-content-planner`, `/seo-content-writer`             | SEO 内容 |
| `seo-technical-optimization` | `/seo-structure-architect`, `/seo-keyword-strategist`     | 技术 SEO |

#### 内容营销

| Plugin              | 主要 Skills                           | 用途         |
|---------------------|---------------------------------------|--------------|
| `content-marketing` | `/content-marketer`                   | 内容营销策略 |
| `marketing-skills`  | `/copywriting`, `/copy-editing`       | 文案撰写     |
| `marketing-skills`  | `/launch-strategy`, `/social-content` | 发布策略     |

#### 增长优化

| Plugin             | 主要 Skills                                        | 用途              |
|--------------------|----------------------------------------------------|-------------------|
| `marketing-skills` | `/pricing-strategy`, `/referral-program`           | 定价、推荐计划     |
| `marketing-skills` | `/analytics-tracking`, `/ab-test-setup`            | 数据追踪、A/B 测试 |
| `marketing-skills` | `/page-cro`, `/signup-flow-cro`, `/onboarding-cro` | 转化优化          |

---

## 通用能力插件

以下插件跨阶段使用，提供通用能力支持：

### Git 工作流

| Plugin                 | Skills                                      | 用途             |
|------------------------|---------------------------------------------|------------------|
| `commit-commands`      | `/commit`, `/commit-push-pr`, `/clean_gone` | Git 提交、PR 创建 |
| `git-pr-workflows`     | `/git-workflow`                             | Git 工作流编排   |
| `developer-essentials` | `/git-advanced-workflows`                   | 高级 Git 操作    |

### 文档与知识管理

| Plugin                     | Skills                                                     | 用途                  |
|----------------------------|------------------------------------------------------------|-----------------------|
| `documentation-generation` | `/docs-architect`, `/api-documenter`, `/tutorial-engineer` | 文档生成              |
| `code-documentation`       | `/code-reviewer`, `/docs-architect`                        | 代码文档              |
| `claude-mem`               | `/mem-search`                                              | 持久化记忆、上下文保存 |

### 调试与诊断

| Plugin              | Skills                          | 用途            |
|---------------------|---------------------------------|-----------------|
| `debugging-toolkit` | `/debugger`, `/dx-optimizer`    | 调试、开发者体验 |
| `error-debugging`   | `/debugger`, `/error-detective` | 错误调试        |
| `error-diagnostics` | `/debugger`, `/error-detective` | 错误诊断        |

### 多 Agent 协作

| Plugin        | Skills                                         | 用途              |
|---------------|------------------------------------------------|-------------------|
| `agent-teams` | `/team-feature`, `/team-debug`, `/team-review` | 多 Agent 并行开发 |
| `agent-teams` | `/team-delegate`, `/team-status`               | 任务协调          |

### 任务管理

| Plugin      | Skills                                       | 用途         |
|-------------|----------------------------------------------|--------------|
| `conductor` | `/setup`, `/status`, `/implement`, `/revert` | 项目任务管理 |
| `conductor` | `/new-track`, `/manage`                      | 任务追踪     |

### Shell 脚本

| Plugin            | Skills                                                    | 用途           |
|-------------------|-----------------------------------------------------------|----------------|
| `shell-scripting` | `/bash-pro`, `/posix-shell-pro`, `/bats-testing-patterns` | Shell 脚本编写 |

### 网页抓取

| Plugin      | Skills           | 用途     |
|-------------|------------------|----------|
| `firecrawl` | `/firecrawl-cli` | 网页爬取 |

---

## 常用 Skills 参考

### Git 工作流

| Skill             | 说明                 |
|-------------------|----------------------|
| `/commit`         | 创建 Git 提交        |
| `/commit-push-pr` | 提交、推送并创建 PR   |
| `/clean_gone`     | 清理已删除的远程分支 |
| `/git-workflow`   | Git 工作流编排       |

### 代码审查

| Skill              | 说明            |
|--------------------|-----------------|
| `/full-review`     | 全面代码审查    |
| `/go-review`       | Go 代码审查     |
| `/python-review`   | Python 代码审查 |
| `/security-review` | 安全漏洞审查    |

### 测试驱动开发

| Skill        | 说明               |
|--------------|--------------------|
| `/tdd`       | TDD 工作流         |
| `/tdd-red`   | 编写失败测试       |
| `/tdd-green` | 实现代码使测试通过 |
| `/tdd-cycle` | 完整 TDD 循环      |

### 规划与设计

| Skill                  | 说明           |
|------------------------|----------------|
| `/plan`                | 复杂任务规划   |
| `/design-system-setup` | 设计系统初始化 |
| `/c4-context`          | C4 上下文图    |

### AI 开发

| Skill                 | 说明             |
|-----------------------|------------------|
| `/prompt-optimize`    | 优化 AI 提示词   |
| `/rag-implementation` | RAG 系统实现     |
| `/ai-assistant`       | 构建 AI 助手应用 |

### 商业分析

| Skill                    | 说明         |
|--------------------------|--------------|
| `/market-opportunity`    | 市场机会分析 |
| `/business-case`         | 商业案例     |
| `/financial-projections` | 财务预测     |

---

## 常见问题

### 配置不生效

1. 关闭所有 Claude Code 窗口
2. 打开新的终端窗口
3. 重新运行 `claude` 启动

### 如何切换模型

使用智谱自动化助手或编辑 `~/.claude/settings.json`：

```json
{
  "env": {
    "ANTHROPIC_DEFAULT_HAIKU_MODEL": "glm-4.5-air",
    "ANTHROPIC_DEFAULT_SONNET_MODEL": "glm-5",
    "ANTHROPIC_DEFAULT_OPUS_MODEL": "glm-5"
  }
}
```

### 版本更新

```bash
claude --version      # 检查当前版本
claude update         # 更新到最新版本
```

---

## 参考链接

- [Claude Code 官方文档](https://docs.anthropic.com/zh-CN/docs/claude-code/overview)
- [智谱 AI Claude Code 配置指南](https://docs.bigmodel.cn/cn/guide/develop/claude)
- [智谱 AI 开放平台](https://open.bigmodel.cn/)
- [MCP 协议文档](https://modelcontextprotocol.io/)

---

## 快速配置清单

按需选择安装的插件组合：

### 最小配置 (个人开发)

```text
claude-mem, context7, commit-commands, debugging-toolkit
```

### 标准配置 (团队开发)

```text
claude-mem, context7, commit-commands, conductor, tdd-workflows,
code-review-ai, debugging-toolkit, documentation-generation
```

### 全栈开发

```text
frontend-mobile-development, backend-development, python-development,
javascript-typescript, api-scaffolding, database-design, tdd-workflows
```

### DevOps 配置

```text
cicd-automation, cloud-infrastructure, kubernetes-operations,
deployment-strategies, performance-testing-review
```

### 创业团队配置

```text
startup-business-analyst, marketing-skills, seo-content-creation,
content-marketing, comprehensive-review, full-stack-orchestration
```
