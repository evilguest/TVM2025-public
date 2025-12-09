import { parseFunnier } from './parser';
import { resolveModule } from './resolver';

export * from './funnier';
export * from './parser';
export * from './resolver';

export function parseAndResolve(source: string, origin?: string) {
    return resolveModule(parseFunnier(source, origin));
}