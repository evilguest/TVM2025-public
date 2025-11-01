import { Expr } from "../../lab04";
import { cost } from "./cost";

export function simplify(e: Expr, identities: [Expr, Expr][]): Expr {
  // Расширяем список тождеств, добавляя обратные направления
  const extendedIdentities: [Expr, Expr][] = [];
  for (const [left, right] of identities) {
    extendedIdentities.push([left, right]);
    extendedIdentities.push([right, left]); // Добавляем обратное направление
  }

  // Множество для отслеживания уже виденных выражений (для предотвращения циклов)
  const seen = new Set<string>();
  
  return simplifyRecursive(e, extendedIdentities, seen);
}

function simplifyRecursive(
  e: Expr, 
  identities: [Expr, Expr][], 
  seen: Set<string>
): Expr {
  // Сначала выполняем свёртку констант
  e = constantFold(e);
  
  // Проверяем, не видели ли мы уже это выражение (предотвращение циклов)
  const exprString = stringifyExpr(e);
  if (seen.has(exprString)) {
    return e; // Уже видели - возвращаем текущую версию
  }
  seen.add(exprString);

  let current = e;
  let improved = true;
  const COST_INCREASE_LIMIT = 5;
  
  // Применяем тождества пока есть улучшения
  while (improved) {
    improved = false;
    let bestExpr = current;
    let bestCost = cost(current);
    
    // Пробуем применить все тождества ко всем подвыражениям
    const candidates = findAllApplications(current, identities);
    
    for (const candidate of candidates) {
      const candidateCost = cost(candidate);
      
      // Разрешаем временное увеличение стоимости
      // Но предпочитаем выражения с меньшей стоимостью
      if (candidateCost < bestCost || 
          (candidateCost === bestCost && !areExprsEqual(candidate, bestExpr))) {
        bestExpr = candidate;
        bestCost = candidateCost;
        improved = true;
      }
    }
    
    if (improved) {
      current = simplifyRecursive(bestExpr, identities, new Set(seen));
    }
  }
  
  return current;
}

// Свёртка констант
function constantFold(e: Expr): Expr {
  switch (e.kind) {
    case "Int":
    case "Ident":
      return e;
      
    case "Neg":
      const foldedInner = constantFold(e.expr);
      if (foldedInner.kind === "Int") {
        return { kind: "Int", value: -foldedInner.value };
      }
      return { kind: "Neg", expr: foldedInner };
      
    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
      const leftFolded = constantFold(e.left);
      const rightFolded = constantFold(e.right);
      
      // Если оба аргумента - константы, вычисляем результат
      if (leftFolded.kind === "Int" && rightFolded.kind === "Int") {
        const leftVal = leftFolded.value;
        const rightVal = rightFolded.value;
        
        switch (e.kind) {
          case "Add":
            return { kind: "Int", value: leftVal + rightVal };
          case "Sub":
            return { kind: "Int", value: leftVal - rightVal };
          case "Mul":
            return { kind: "Int", value: leftVal * rightVal };
          case "Div":
            // Избегаем деления на ноль
            if (rightVal === 0) return { kind: "Div", left: leftFolded, right: rightFolded };
            return { kind: "Int", value: leftVal / rightVal };
        }
      }
      
      return {
        kind: e.kind,
        left: leftFolded,
        right: rightFolded
      };
  }
}

// Находит все возможные применения тождеств ко всем подвыражениям
function findAllApplications(e: Expr, identities: [Expr, Expr][]): Expr[] {
  const results: Expr[] = [];
  
  // Пробуем применить каждое тождество к текущему выражению
  for (const [pattern, replacement] of identities) {
    const applied = applyIdentity(e, pattern, replacement);
    if (applied && !areExprsEqual(applied, e)) {
      results.push(applied);
    }
  }
  
  // Рекурсивно применяем к подвыражениям
  switch (e.kind) {
    case "Int":
    case "Ident":
      break;
      
    case "Neg":
      for (const childResult of findAllApplications(e.expr, identities)) {
        results.push({ kind: "Neg", expr: childResult });
      }
      break;
      
    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
      for (const leftResult of findAllApplications(e.left, identities)) {
        results.push({ kind: e.kind, left: leftResult, right: e.right });
      }
      for (const rightResult of findAllApplications(e.right, identities)) {
        results.push({ kind: e.kind, left: e.left, right: rightResult });
      }
      break;
  }
  
  return results;
}

// Применяет одно тождество к выражению
function applyIdentity(expr: Expr, pattern: Expr, replacement: Expr): Expr | null {
  const mapping = new Map<string, Expr>();
  
  if (matchPattern(expr, pattern, mapping)) {
    return substitute(replacement, mapping);
  }
  
  return null;
}

// Проверяет соответствие выражения шаблону и строит mapping переменных
function matchPattern(expr: Expr, pattern: Expr, mapping: Map<string, Expr>): boolean {
  if (pattern.kind === "Ident") {
    // Шаблон - переменная
    const existingMapping = mapping.get(pattern.name);
    if (existingMapping) {
      // Переменная уже встречалась - проверяем совпадение
      return areExprsEqual(expr, existingMapping);
    } else {
      // Новая переменная - добавляем в mapping
      mapping.set(pattern.name, expr);
      return true;
    }
  }
  
  // Проверяем, что виды выражений совпадают
  if (expr.kind !== pattern.kind) return false;
  
  // Рекурсивная проверка для составных выражений
  switch (expr.kind) {
    case "Int":
      return (pattern as any).value === expr.value;
      
    case "Neg":
      return matchPattern(expr.expr, (pattern as any).expr, mapping);
      
    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
      const patternBinary = pattern as any;
      return matchPattern(expr.left, patternBinary.left, mapping) &&
             matchPattern(expr.right, patternBinary.right, mapping);
             
    default:
      return false;
  }
}

// Заменяет переменные в выражении согласно mapping
function substitute(expr: Expr, mapping: Map<string, Expr>): Expr {
  switch (expr.kind) {
    case "Int":
      return expr;
      
    case "Ident":
      return mapping.get(expr.name) || expr;
      
    case "Neg":
      return {
        kind: "Neg",
        expr: substitute(expr.expr, mapping)
      };
      
    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
      return {
        kind: expr.kind,
        left: substitute(expr.left, mapping),
        right: substitute(expr.right, mapping)
      };
  }
}

// Проверяет структурное равенство выражений
function areExprsEqual(a: Expr, b: Expr): boolean {
  if (a.kind !== b.kind) return false;
  
  switch (a.kind) {
    case "Int":
      return a.value === (b as any).value;
      
    case "Ident":
      return a.name === (b as any).name;
      
    case "Neg":
      return areExprsEqual(a.expr, (b as any).expr);
      
    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
      return areExprsEqual(a.left, (b as any).left) &&
             areExprsEqual(a.right, (b as any).right);
  }
}

// Создает строковое представление выражения для отслеживания циклов
function stringifyExpr(e: Expr): string {
  switch (e.kind) {
    case "Int":
      return `Int(${e.value})`;
    case "Ident":
      return `Ident(${e.name})`;
    case "Neg":
      return `Neg(${stringifyExpr(e.expr)})`;
    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
      return `${e.kind}(${stringifyExpr(e.left)},${stringifyExpr(e.right)})`;
  }
}