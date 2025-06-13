import os
import subprocess
import sys
import csv

# Powered by GPT-4o!
# KNU COMP0321: Compiler Design

def run_test(minic_path, test_input_path, test_type='-t'):
    try:
        result = subprocess.run(
            [sys.executable, minic_path, test_input_path, test_type, "test_result.txt"],
            capture_output=True,
            text=True,
            timeout=5
        )
        ast_fp = open('./test_result.txt','r')
        ast_result = ast_fp.read()
        #return result.stdout.strip().splitlines()
        return ast_result.strip().splitlines()
    except Exception as e:
        return [f"[ERROR] Failed to run {minic_path} with {test_input_path}: {str(e)}"]

def draw_ast(minic_path, test_input_path, fileName):
    try:
        result = subprocess.run(
            [sys.executable, minic_path, test_input_path, '-ast', fileName],
            capture_output=True,
            text=True,
            timeout=5
        )
    except Exception as e:
        return [f"[ERROR] Failed to run {minic_path} with {test_input_path}: {str(e)}"]

def load_expected_output(output_path):
    try:
        with open(output_path, 'r', encoding='utf-8') as f:
            return f.read().strip().splitlines()
    except Exception as e:
        return [f"[ERROR] Failed to load expected output: {str(e)}"]

def compare_outputs(expected, actual):
    return expected == actual

def count_differences(expected, actual):
    count = 0
    max_len = max(len(expected), len(actual))
    for i in range(max_len):
        exp = expected[i] if i < len(expected) else "<no line>"
        act = actual[i] if i < len(actual) else "<no line>"
        if exp != act:
            count += 1
    return count

def show_differences(expected, actual):
    print("{:<7} {:<70} | {:<70}".format("Line", "Expected", "Actual"))
    print("-" * 150)
    max_len = max(len(expected), len(actual))
    for i in range(max_len):
        exp = expected[i] if i < len(expected) else "<no line>"
        act = actual[i] if i < len(actual) else "<no line>"
        if exp != act:
            print(f"{i+1:<7} {exp:<70} | {act:<70}")

def grade_all_tests(minic_path, testcase_dir='tst/base/AST_testcases', solution_dir='tst/base/AST_solutions_trees', extension='.ast'):
    total = 0
    passed = 0
    results = []

    # You can set the size of testcases (default: 80)
    for i in range(1, 43):
        test_filename = f'c{i}.mc'
        solution_filename = f'c{i}.mc' + extension

        test_file = os.path.join(testcase_dir, test_filename)
        solution_file = os.path.join(solution_dir, solution_filename)

        if not os.path.exists(test_file) or not os.path.exists(solution_file):
            print(f"⚠️  Test #{i}: Skipped (file missing)")
            print(f"  → 🔍 Looking for: {os.path.abspath(test_file)}  → Exists? {os.path.exists(test_file)}")
            print(f"  → 🔍 Looking for: {os.path.abspath(solution_file)}  → Exists? {os.path.exists(solution_file)}")
            continue
        
        if extension == '.ast':
            student_output = run_test(minic_path, test_file)
        elif extension == '.u':
            student_output = run_test(minic_path, test_file, '-u')
        expected_output = load_expected_output(solution_file)

        if compare_outputs(expected_output, student_output):
            print(f"✅ Test #{i}: Passed")
            passed += 1
            results.append([test_filename, 'Passed', 0])
        else:
            diff_count = count_differences(expected_output, student_output)
            print(f"❌ Test #{i}: Failed ({diff_count} differences)")
            show_differences(expected_output, student_output)
            results.append([test_filename, 'Failed', diff_count])
            print()

        total += 1

def gen_ast_images(minic_path, testcase_dir='tst/base/AST_testcases', solution_dir='AST_images'):
    total = 0
    passed = 0
    results = []

    # You can set the size of testcases (default: 80)
    for i in range(1, 43):
        test_filename = f'c{i}.mc'
        test_file = os.path.join(testcase_dir, test_filename)

        if not os.path.exists(test_file):
            print(f"⚠️  Test #{i}: Skipped (file missing)")
            print(f"  → 🔍 Looking for: {os.path.abspath(test_file)}  → Exists? {os.path.exists(test_file)}")
            continue
        
        draw_ast(minic_path, test_file, solution_dir+'/'+f'c{i}')
        
        print(f"✅ AST Image #{i}: Generated!")
        passed += 1
        
        total += 1

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="Auto-grader for MiniC Scanner")
    parser.add_argument("--minic", type=str, default="MiniC.py", help="Path to MiniC.py")
    parser.add_argument("--testcase_dir", type=str, default="tst/base/AST_testcases", help="Path to test case inputs")
    parser.add_argument("--solution_dir", type=str, default="tst/base/AST_solutions_trees", help="Path to expected outputs")
    args = parser.parse_args()

    print('************** [AST Printer Test] **************')
    grade_all_tests(args.minic, args.testcase_dir, args.solution_dir)
    print('\n\n')
    print('*************** [Unparser Test] ****************')
    grade_all_tests(args.minic, args.testcase_dir, "tst/base/AST_solutions_unparsed", '.u')

    print()
    print('[AST Visualization]')
    gen_ast_images(args.minic, args.testcase_dir, 'AST_images')
