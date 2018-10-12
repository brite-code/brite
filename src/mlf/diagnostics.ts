const opaque = Symbol();

export type ReportedDiagnostic<T> = T & typeof opaque;

export class Diagnostics {
  static report<T>(
    diagnostics: Diagnostics,
    diagnostic: T
  ): ReportedDiagnostic<T> {
    diagnostics.reported.push(diagnostic);
    return diagnostic as ReportedDiagnostic<T>;
  }

  private readonly reported: Array<unknown> = [];
}
