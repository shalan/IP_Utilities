
## IP Wrapper generator for AMBA APB and AHB Lite

``amba_wrap.py -apb|-ahb -tb|-ch ip.yml``
- Options:
    - `-apb` : generate APB wrapper
    - `-ahb` : generate AHB wrapper
    - `-tb` : generate a Verilog testbench for the generated bus wrapper
    - `-ch` : generate a C header file containing the register definitions
- Arguments:
    - `ip.yml`: A YAML file that contains the IP definition

## YAML Template Generator

``v2yaml.py verilog_file module_name``

## YAML IP Definition File Format

A YAML file is used to capture the IP information. This includes:
### info
Basic information about the IP like the author, the license, etc. For an example:
```yaml
info: 
  name: "MS_GPIO"
  description: "An 8-bit bi-directional General Purpose I/O (GPIO) with synchronizers and edge detectors."
  repo: "github.com/shala/MS_GPIO"
  owner: "AUCOHL"
  license: "MIT"
  author: "Mohamed Shalan"
  email: "mshalan@aucegypt.edu"
  version: "v1.0.0"
  date: "3-18-2022"
  category: "digital"
  tags: 
    - peripheral
    - GPIO
  bus: 
    - generic
  type": "soft"
  status: "verified"
  cell_count: "690"
  width": "0.0"
  height": "0.0"
  technology: "n/a"
  clock_freq_mhz: "10"
  digital_supply_voltage: "n/a"
  analog_supply_voltage: "n/a"
  ```

### registers
Register definitions. For an example:
```yaml
registers:
  - name: "data"
    size: 8
    mode: "rw"
    offset: 0
    bit_access: no
    read_port: "data_in"
    write_port: "data_out"
    description: "Data register."
  - name: "out_en"
    size: 8
    mode: "w"
    offset: 4
    bit_access: no
    write_port: "out_en"
    description: "Output enable register; used to set the direction: 1-out, 0-in"
```
The ``mode`` property can be set to: 
- ``w`` for registers that are meant for writing only; reading from it returns the last written data value.
- ``r`` for registers that are meant for reading only; hence they cannot be written. 
- ``rw`` for registers that are read and written differently; for an example, the data register of a GPIO peripheral. Reading this register returns the data provided on input GPIO pins and writting the register sets the values of output GPIO pins.

The ``bit_access`` property is used to enable bit level access.

### ports
IP Port definitions. For an example:
```yaml
ports:
  - name: "data_in"
    width: 8
  - name: "data_out"
    width: 8
  - name: "data_out_en"
    width: 8
  - name: "pad_in"
    width: 8
  - name: "pad_out"
    width: 8
  - name: "pad_out_en"
    width: 8
  - name: "flags_pe"
    width: 8
  - name: "flags_ne"
    width: 8
  - name: "flags_lo"
    width: 8
  - name: "flags_hi"
    width: 8
```
### external_interface
IP External Interfaces to other sub-systems. For an example:
```yaml
external_interface: 
  - name: "i_pad_in"
    port: "pad_in"
    direction: "input"
    width: 8
  - name: "o_pad_out"
    port: "pad_out"
    direction: "output"
    width: 8
  - name: "o_pad_out_en"
    port: "pad_out_en"
    direction: "output"
    width: 8
```
### flags
Event flags used for generating interrupts. For an example:
```yaml
flags: 
  - name: "pe"
    port: "flags_pe"
  - name: "ne"
    port: "flags_ne"
  - name: "lo"
    port: "flags_lo"
  - name: "hi"
    port: "flags_hi"
```

