import { parseAndResolve } from '../src';
import { testFilesInFolder } from '../../lab08/tests/testFilesInFolder';


describe('10. Testing the sample files', () => {
    testFilesInFolder("./lab10/samples", parseAndResolve);
});
