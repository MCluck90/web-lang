export class HTMLModule {
  constructor(
    public readonly moduleName: string,
    public readonly contents: string
  ) {}
}

export class JSModule {
  constructor(
    public readonly moduleName: string,
    public readonly contents: string
  ) {}
}

export interface CompilerConfig {
  projectName: string
}

export interface CompilerOutput {
  html: HTMLModule[]
  isomorphicJs: JSModule[]
}
export type Output = CompilerOutput
