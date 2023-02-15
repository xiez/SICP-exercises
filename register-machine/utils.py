from parser import parse

def get_controller_text(test_file):
    controller_file = './test-cases/' + test_file.split('test_')[1].replace('.py', '.ss')
    with open(controller_file, 'r') as f:
        controller_file_cont = f.read()
        return parse(controller_file_cont)
