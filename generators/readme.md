# BusWrap

A set of Python utilities as well as Verilog macros to help with IP development. 

## IP wrapper generator 
For APB, AHB Lite and Wishbone buses. It converts an IP description in YAML (or JSON) to a Verilog RTL of the bus interface including all I/O registers as well as the necessary control logic. 

``python3 bus_wrap.py ip.yml|ip.json -apb|-ahbl|-wb -tb|-ch|-md``
- Options:
    - `-apb` : generates an APB wrapper.
    - `-ahbl` : generates an AHB Lite wrapper.
    - `-wb` : generates a WB wrapper.
    - `-tb` : generates a Verilog testbench for the generated bus wrapper.
    - `-ch` : generates a C header file containing the register definitions.
    - `-md` : generates documentation in MD and Bitfield formats.
    - `-dft` : generate wrapper for dft.
- Arguments:
    - `ip.yaml|ip.json`: A YAML/JSON file that contains the IP definition.

## YAML Template Generator
Generates a YAML template of the IP given its Verilog RTL source file.

``python3 v2yaml.py IP.v module_name``

## A Typical Workflow

1. Describe the IP in YAML or JSON format. The format is outlined in the following section. To make things easier, ```v2yaml.py``` may be used to generate a template YAML file from the IP RTL Verilog file with some of the sections filled automatically for you.

2. [**Optional**] Convert the YAML file into JSON using tools such as [this one](https://onlineyamltools.com/convert-yaml-to-json). *Perform this step if you prefer dealing with JSON instead of YAML format.*

3. Generate the wrapper RTL by invoking: 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
``python3 bus_wrap.py ip.yml|ip.json -apb|-ahbl|-wb > ip_APB.v``

4. Genertate a template testbench.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
``python3 bus_wrap.py ip.yml|ip.json -apb|-ahbl|-wb -tb > ip_APB_tb.v``

5. Generate the Register Definitions C header file needed for FW development.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
``python3 bus_wrap.py ip.yml|ip.json -apb|-ahbl|-wb -ch > ip_APB.h``

6. Generate an, almost, complete Markdown documentation of the IP. This includes register/fields tables as well as graphics for the register fields in the bitfield format.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
``python3 bus_wrap.py ip.yml|ip.json -apb|-ahbl|-wb -md > ip_APB.md``

## YAML IP Definition File Format

A YAML file is used to capture the IP information for the sake of generating the RTL bus wrappers including I/O registers. This includes:

> *Note:* The key/property ``port`` always means a port in the IP to wrap.

### General Information

Basic information about the IP like the author, the license, etc. For an example:
```yaml
info: 
  name: "MS_GPIO"
  description: "An 8-bit bi-directional General Purpose I/O (GPIO) with synchronizers and edge detectors."
  repo: "github.com/shalan/MS_GPIO"
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
  cell_count: 
    - IP: 797
    - APB: 1435
    - AHBL: 1501
    - WB: 0
  width": 0.0
  height": 0.0
  technology: "n/a"
  clock_freq_mhz:
    - IP: 163
    - APB: 135
    - AHBL: 128
    - WB: 0
  digital_supply_voltage: "n/a"
  analog_supply_voltage: "n/a"
  static_power: 0.0,
  dynamic_power: 0.0
  irq_reg_offset: 0xFF00
  fifo_reg_offset: 0xFE00
  ```

### Parameter Definitions

This section is used for soft digital IPs if the IP RTL model is parameterized. The parameters defined in this section can be used in other sections to specify the widths of fields and registers.

```YAML
parameters:
  - name: SC
    default: 8
  - name: MDW
    default: 9
  - name: GFLEN
    default: 8
  - name: FAW
    default: 4
```
### Port Definitions

IP Port definitions. The Verilog ports it is a digital soft IP or the macro pins if hard macro. For example:
```yaml
ports:
  - name: "data_in"
    width: 8
    direction: input
    description: "The input data"
  - name: "data_out"
    width: 8
    direction: output
    description: "The output data"
```

> *Note:* The CLock, Rest and External Interfaces must not be listed here. 

### External Interface Definitions
IP External Interfaces to other sub-systems. In other words, the IP ports that pass through the bus wrapper to outside the wrapper; typically, the IP ports connected to I/Os. For example:

```yaml
external_interface: 
  - name: "i_pad_in"
    port: "pad_in"
    direction: "input"
    width: 8
    description: The input pads
  - name: "o_pad_out"
    port: "pad_out"
    direction: "output"
    width: 8
    description: The output pads
```
If the external interface needs a synchronizer, add the ```sync``` property and set to ```True```. For example:
```yaml
external_interface:
  - name: sdi
    port: sdi
    direction: input
    sync: True
    width: 1
    description: The input serial data 
```

> *Note:* If you need to drive an external interface using an I/O register field, just create passthrough ports in the wrapped IP. One of these ports is input and the other port is output. Connect the two ports inside the wrapped IP (e.e., using ``assign`` statement in Verilog). Set the ``port`` property in the field to the input port and the ``external_interface`` port to the output port.

### Clock and Reset Definitions

The Clock and Reset ports definition. You must not list them under the ports section.

```YAML
clock:
  name: clk

reset:
  name: rst_n
  level: 0
```

`level`: determines the edge, 0: Negative, 1: Positive
### Register Definitions

Register definitions. For example:
```yaml
registers:
- name: CAP
  size: 16
  mode: r
  fifo: no
  offset: 8
  bit_access: no
  read_port: capture
  description: The captured value.
- name: CTRL
  size: 2
  mode: w
  fifo: no
  offset: 12
  bit_access: no
  description: Control Register.
  fields:
  - name: TE
    bit_offset: 0
    bit_width: 1
    write_port: tmr_en
    description: Timer enable
  - name: CE
    bit_offset: 1
    bit_width: 1
    write_port: cntr_en
    description: Counter enable
```
- The ``mode`` property can be set to: 
  - ``w`` for registers that are meant for writing only; reading from it returns the last written data value.
  - ``r`` for registers that are meant for reading only; hence they cannot be written. 
  - ``rw`` for registers that are read and written differently; for example, the data register of a GPIO peripheral. Reading this register returns the data provided on input GPIO pins and writing the register sets the values of output GPIO pins.

- The ``bit_access`` property is used to enable bit-level access (**Not Implemented**).
- The ``fifo`` property is used to specify whether this register is used to access a FIFO. If it is set to ``yes`` the FIFO has to be defined.
- The ``auto_clear`` property is used to clear the field to 0 after writing to it 1.

### FIFO Definitions
This section is used if the IP has internal data FIFOs. This is typically the case of IPs that deal with data streams such as UART. Data received by the IP is placed into a ``receive`` FIFO by the IP and data to be sent is placed by the bus wrapper into the ``transmit`` FIFO. For each FIFO, you need to specify:
- `type` : `read` (receive) or `write` (transmit)
- The FIFO has 2^`address_width` number of words, each is `width` bits.
- The `register` is used to access the FIFO in firmware
- The `data_port` is used to provide the data to `write` FIFO or read the data from `read` FIFO.
- The `control_port` is used to specify the ports used to control the FIFO read or write operations.
- The `flush_enable` and `flush_port` are used to enable the FIFO flush feature and set the port if enabled.
- The `threshold_port` is used to specify the level threshold port
- The `level_port` is used to specify the level port

```yaml
fifos:
  - name: RX_FIFO
    type: read  
    width: DW
    address_width: AW 
    register: RXDATA
    data_port: rdata
    control_port: rd
    flush_enable: True
    flush_port: rx_fifo_flush
    threshold_port: rxfifotr
    level_port: rx_level
  - name: TX_FIFO
    type: write
    width: DW
    depth: AW
    register: TXDATA
    data_port: wdata
    control_port: wr
    flush_enable: True
    flush_port: tx_fifo_flush
    threshold_port: txfifotr
    level_port: tx_level
```

> *Note:* In this example, `MDW` is a module parameter.

The bus wrapper generator automatically generates the following registers for the fifo:
|Register | Mode | Function  |
|---|---|---
|```LEVEL_REG```| R| The FIFO current level (Number of data words in the FIFO).|
|```THRESHOLD_REG```| W| The level above which the FIFO generates an interrupt.|
|```FLUSH_REG```| W| Writing any value to this register flushes the FIFO. This register is auto-cleared.|

### Event Flags Definition
Event flags are used for generating interrupts. For an example:

```yaml
flags:
- name: CM
  port: cntr_match
  description: Counter match.
- name: CAP
  port: cap_done
  description: Capture is done.
```
The bus wrapper generator generates four different registers to manage interrupts: ``RIS`` (Raw Interrupt Status), ``IM`` (Interrupt Mask), ``MIS`` (Masked Interrupt Status) and ``IC`` (Interrupt Clear). Each bit in each represents one of the flags.

|Register | Mode | Function  |
|---|---|---
|```RIS``` | Read-only (R)| Has the interrupt flags before masking  |
|```IM```  | Writable (W)| Interrupt Masking, clearing a bit disables the corresponding interrupt  |
|```MIS``` | Read-only (R)| Masked Interrupt status, the outcome of bitwise-ANDing RIS and IM |
|```IC``` | Writable (W)| Interrupt Flag Clear, setting a bit clears the corresponding interrupt flag |
