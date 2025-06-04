#!/usr/bin/env python3
"""
Gerador de wrapper Delphi para Scintilla a partir do arquivo .iface
Uso: python delphi_generator.py Scintilla.iface
"""

import sys
import re
from collections import OrderedDict

class DelphiGenerator:
    def __init__(self):
        self.functions = []
        self.constants = OrderedDict()
        self.enums = OrderedDict()
        self.events = []
        
    def parse_iface(self, filename):
        """Parseia o arquivo .iface"""
        with open(filename, 'r', encoding='utf-8') as f:
            lines = f.readlines()
            
        current_category = ""
        current_enum = None
        
        for line in lines:
            line = line.strip()
            
            # Ignora linhas vazias e comentários
            if not line or line.startswith('#'):
                continue
                
            # Categoria
            if line.startswith('cat '):
                current_category = line[4:]
                continue
                
            # Função
            if line.startswith('fun ') or line.startswith('get ') or line.startswith('set '):
                self.parse_function(line, current_category)
                continue
                
            # Constante
            if line.startswith('val '):
                self.parse_constant(line, current_enum)
                continue
                
            # Enumeração
            if line.startswith('enu '):
                current_enum = self.parse_enum(line)
                continue
                
            # Evento
            if line.startswith('evt '):
                self.parse_event(line)
                continue
    
    def parse_function(self, line, category):
        """Parseia uma definição de função"""
        # Regex para extrair partes da função
        match = re.match(r'(fun|get|set)\s+(\w+)\s+(\w+)=(\d+)\((.*?)\)', line)
        if match:
            func_type = match.group(1)
            return_type = match.group(2)
            name = match.group(3)
            msg_id = match.group(4)
            params = match.group(5)
            
            # Converte tipos C++ para Delphi
            delphi_return = self.convert_type(return_type)
            
            # Parseia parâmetros
            param_list = []
            if params.strip():
                for param in params.split(','):
                    param = param.strip()
                    if ' ' in param:
                        ptype, pname = param.rsplit(' ', 1)
                        param_list.append({
                            'type': self.convert_type(ptype),
                            'name': pname
                        })
            
            self.functions.append({
                'name': name,
                'msg_id': msg_id,
                'return_type': delphi_return,
                'params': param_list,
                'category': category,
                'type': func_type
            })
    
    def parse_constant(self, line, enum_name):
        """Parseia uma constante"""
        match = re.match(r'val\s+(\w+)=(.+)', line)
        if match:
            name = match.group(1)
            value = match.group(2)
            
            if enum_name:
                if enum_name not in self.enums:
                    self.enums[enum_name] = []
                self.enums[enum_name].append((name, value))
            else:
                self.constants[name] = value
    
    def parse_enum(self, line):
        """Parseia uma declaração de enumeração"""
        match = re.match(r'enu\s+(\w+)=(\w+)', line)
        if match:
            return match.group(1)
        return None
    
    def parse_event(self, line):
        """Parseia uma definição de evento"""
        match = re.match(r'evt\s+(\w+)\s+(\w+)=(\d+)\((.*?)\)', line)
        if match:
            return_type = match.group(1)
            name = match.group(2)
            event_id = match.group(3)
            params = match.group(4)
            
            self.events.append({
                'name': name,
                'event_id': event_id,
                'params': params
            })
    
    def convert_type(self, cpp_type):
        """Converte tipos C++ para Delphi"""
        type_map = {
            'void': '',
            'int': 'Integer',
            'bool': 'Boolean',
            'position': 'Integer',
            'colour': 'TColor',
            'string': 'PAnsiChar',
            'stringresult': 'PAnsiChar',
            'cells': 'PAnsiChar',
            'textrange': 'PTextRange',
            'findtext': 'PFindText',
            'keymod': 'Integer',
            'formatrange': 'PFormatRange'
        }
        return type_map.get(cpp_type, 'Integer')
    
    def generate_constants(self):
        """Gera as constantes Delphi"""
        output = ["const"]
        
        # Constantes simples
        for name, value in self.constants.items():
            output.append(f"  {name} = {value};")
        
        # Enumerações
        for enum_name, values in self.enums.items():
            output.append(f"\n  // {enum_name}")
            for name, value in values:
                output.append(f"  {name} = {value};")
        
        return '\n'.join(output)
    
    def generate_functions(self):
        """Gera as declarações de funções Delphi"""
        output = []
        current_category = ""
        
        for func in self.functions:
            # Adiciona comentário de categoria
            if func['category'] != current_category:
                current_category = func['category']
                output.append(f"\n    // {current_category}")
            
            # Monta a declaração da função
            func_name = func['name']
            
            # Parâmetros
            params = []
            if func['params']:
                for param in func['params']:
                    if param['name']:
                        params.append(f"{param['name']}: {param['type']}")
            
            param_str = '; '.join(params)
            
            # Declaração completa
            if func['return_type']:
                output.append(f"    function {func_name}({param_str}): {func['return_type']};")
            else:
                output.append(f"    procedure {func_name}({param_str});")
        
        return '\n'.join(output)
    
    def generate_implementations(self):
        """Gera as implementações das funções"""
        output = []
        
        for func in self.functions:
            func_name = func['name']
            msg_id = func['msg_id']
            
            # Parâmetros
            params = []
            if func['params']:
                for param in func['params']:
                    if param['name']:
                        params.append(f"{param['name']}: {param['type']}")
            
            param_str = '; '.join(params)
            
            # Cabeçalho da função
            if func['return_type']:
                output.append(f"\nfunction TScintilla.{func_name}({param_str}): {func['return_type']};")
            else:
                output.append(f"\nprocedure TScintilla.{func_name}({param_str});")
            
            output.append("begin")
            
            # Corpo da função - chamada SendMessage
            wparam = "0"
            lparam = "0"
            
            if func['params']:
                if len(func['params']) >= 1 and func['params'][0]['name']:
                    wparam = func['params'][0]['name']
                if len(func['params']) >= 2 and func['params'][1]['name']:
                    lparam = f"LPARAM({func['params'][1]['name']})"
            
            if func['return_type']:
                output.append(f"  Result := SendMessage(FHandle, {msg_id}, {wparam}, {lparam});")
            else:
                output.append(f"  SendMessage(FHandle, {msg_id}, {wparam}, {lparam});")
            
            output.append("end;")
        
        return '\n'.join(output)
    
    def generate_delphi_unit(self):
        """Gera a unit Delphi completa"""
        unit = f"""unit ScintillaWrapper;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls;

{self.generate_constants()}

type
  TScintilla = class(TCustomControl)
  private
    FHandle: HWND;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
{self.generate_functions()}
  end;

implementation

constructor TScintilla.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Inicialização do Scintilla
end;

destructor TScintilla.Destroy;
begin
  // Limpeza
  inherited Destroy;
end;

{self.generate_implementations()}

end.
"""
        return unit

def main():
    if len(sys.argv) < 2:
        print("Uso: python delphi_generator.py Scintilla.iface")
        sys.exit(1)
    
    generator = DelphiGenerator()
    generator.parse_iface(sys.argv[1])
    
    # Gera o arquivo Delphi
    with open('ScintillaWrapper.pas', 'w', encoding='utf-8') as f:
        f.write(generator.generate_delphi_unit())
    
    print("Arquivo ScintillaWrapper.pas gerado com sucesso!")
    print(f"Total de funções: {len(generator.functions)}")
    print(f"Total de constantes: {len(generator.constants)}")
    print(f"Total de enumerações: {len(generator.enums)}")

if __name__ == "__main__":
    main()
