# Robust Error Handling System for realtalk Package

## Overview

This document outlines a comprehensive error handling system for the `realtalk` R package. The system is designed to address the current limitations in monitoring, capturing, and accessing errors in background processes.

## Goals

1. Provide real-time monitoring of background process health
2. Capture complete STDERR/STDOUT from all processes
3. Enable post-hoc analysis of process failures
4. Implement consistent logging across all components
5. Support graceful failure recovery when possible
6. Present clear, actionable error information to users

## Components

### 1. ManagedProcess (R6 Class)

An R6 class that wraps a background process with integrated output capture and monitoring capabilities.

#### Public Fields
None

#### Private Fields
- `process`: The actual callr process object
- `process_name`: Unique identifier for the process
- `started_at`: Timestamp when process was started
- `status`: Current status ("running" or "dead")
- `last_checked`: When the process was last checked
- `restarts`: Count of how many times process has been restarted
- `exit_status`: Exit code or status (when process has died)
- `stdout_buffer`: Vector of stdout lines with timestamps
- `stderr_buffer`: Vector of stderr lines with timestamps
- `start_time`: When the buffer was created
- `max_size`: Safety limit for extreme cases (default very large)
- `logger`: Reference to Logger instance for logging events

#### Public Methods
- `initialize(process, name, logger)`: Create a new managed process
- `check()`: Check if the process is still alive and update status
- `get_status()`: Get detailed status information about the process
- `restart(restart_function)`: Attempt to restart a dead process
- `get_stdout()`: Get all stdout content
- `get_stderr()`: Get all stderr content
- `get_combined_output()`: Get all process information and outputs

#### Private Methods
- `append_stdout(content)`: Add content to stdout buffer with timestamp
- `append_stderr(content)`: Add content to stderr buffer with timestamp
- `check_buffer_size()`: Safety check to prevent excessive memory usage

#### Functions and Behavior
- Self-contained process management with integrated output buffering
- Process health status is checked on demand 
- Complete stdout and stderr capture without artificial limits
- Preservation of process information even after termination
- Automatic process output timestamping

### 2. ProcessManager (R6 Class)

A centralized system for tracking and monitoring all background processes.

#### Public Fields
None

#### Private Fields
- `logger`: Reference to Logger instance for logging events
- `processes`: Named list of ManagedProcess instances

#### Public Methods
- `initialize(logger)`: Create a new ProcessManager with a reference to a Logger
- `register_process(process, name)`: Create and store a ManagedProcess instance
- `check_processes()`: Check health of all processes and return their statuses
- `check_process(name)`: Check health of a specific process
- `get_error_history(name = NULL)`: Get error history for one or all processes
- `restart_process(name, restart_function)`: Attempt to restart a dead process

#### Private Methods
None significant

#### Functions and Behavior
- Central registry of all managed processes
- Unified interface for checking process health
- Consistent access to process statuses and output history
- Process registration automatically sets up output capture

### 3. Logger (R6 Class)

A unified logging system with standardized formats and severity levels.

#### Public Fields
None

#### Private Fields
- `log_path`: Path to the log file

#### Public Methods
- `initialize(log_path)`: Create a new Logger
- `info(component, message, context = list())`: Log an informational message
- `warning(component, message, context = list())`: Log a warning message
- `error(component, message, context = list())`: Log an error message
- `log(level, component, message, context = list())`: Direct logging implementation
- `get_logs(level = NULL, component = NULL, pattern = NULL, n = NULL)`: Retrieve filtered log entries

#### Private Methods
- `write_to_log(entry)`: Write entry to log file with thread safety

#### Functions and Behavior
- Three log levels: INFO, WARNING, and ERROR
- WARNING level specifically for R warnings
- Thread-safe logging with file locking
- Structured log format with timestamps, levels, components, and optional context
- Log filtering by level, component, or text pattern
- All messages are logged regardless of level (no minimum level filtering)

### 4. Stream Class Enhancements

The Stream class would be extended to integrate with the error handling system.

#### New Private Fields
- `process_manager`: Instance of ProcessManager
- `logger`: Instance of Logger

#### New Public Methods
- `error_history(process_name = NULL)`: Get error information for one or all processes
- `process_status()`: Get status of all tracked background processes

#### Enhanced Existing Methods
- `start_streaming()`: Now includes process registration and health check integration
- `stop_streaming()`: Enhanced to ensure clean process termination
- `initialize()`: Now creates Logger and ProcessManager instances

#### Main Loop Integration
- The main loop in `start_streaming()` would include health checks on each iteration
- Process recovery would be attempted directly in the main loop
- Process outputs would be captured continuously from streams

## Process Monitoring Flow

1. **Initialization**:
   - Stream object creates ProcessManager and Logger instances
   - Each process is wrapped in a ManagedProcess when created

2. **Process Registration**:
   - Each background process is registered with the ProcessManager upon creation
   - ManagedProcess instances automatically handle output buffering

3. **Main Loop Health Checks**:
   - Main loop calls ProcessManager's check_processes() on each iteration
   - Process outputs are captured and stored continuously
   - R warnings are captured using the WARNING log level

4. **Error Detection and Recovery**:
   - When the main loop detects a process failure:
     - The error is logged with ERROR level
     - Process status is updated to "dead"
     - Recovery is attempted based on process type
     - User is notified through the logging system

5. **Error Reporting**:
   - Users can access error history at any time through Stream$error_history()
   - Complete process outputs are available for inspection

## Log Levels

### INFO
- Normal operational events
- Successful initializations or connections
- Regular status updates
- Standard operational flow

### WARNING
- Potential issues that don't prevent operation
- R warnings captured from any component
- Recoverable errors
- Resource pressure situations
- Degraded functionality scenarios

### ERROR
- Serious problems affecting functionality
- Process crashes or terminations
- Connection failures
- API errors
- Unrecoverable situations

## Logging Format

```
[TIMESTAMP] [LEVEL] [COMPONENT] Message {context_json}
```

Example:
```
[2025-05-13 14:30:22] [ERROR] [sox_bg] Process died unexpectedly {"exit_status": 1, "uptime": "00:03:22"}
```

## Process Status Tracking

ManagedProcess would track these status attributes for each process:

- `status`: Current status ("running" or "dead")
- `started_at`: Timestamp when process was started
- `last_checked`: When the process was last checked
- `restarts`: Count of how many times process has been restarted
- `exit_status`: Exit code or status (when process has died)

## Error Recovery Strategy

The system implements a targeted approach to error recovery:

1. **Process-Specific Recovery**:
   - Each process type has specialized recovery procedures
   - Recovery functions are provided to restart_process() method
   - State preservation is attempted where possible

2. **Degraded Operation**:
   - System can continue in degraded state if non-critical processes fail
   - Critical process failures trigger controlled shutdown

3. **User Notification**:
   - Clear error messages indicate what failed and why
   - Information about recovery attempts is logged

## Integration with Stream Class

The Stream class would integrate with the error handling system by:

1. Creating and maintaining ProcessManager and Logger instances
2. Registering background processes as they are created
3. Performing health checks in the main loop
4. Attempting recovery of failed processes
5. Providing user-facing methods for error inspection

## Error History Access

The error history system provides:

1. Access to complete stdout and stderr from any process, even after termination
2. Process metadata including start time, status, and exit code
3. Filtering capability to isolate specific processes or types of errors
4. Structured output format for programmatic analysis

## Backward Compatibility

The error handling system is designed for backward compatibility:

1. Existing logging mechanisms would forward to the new Logger
2. Current process initialization would automatically register with ProcessManager
3. Error handling would enhance existing functionality without breaking changes

## Conclusion

This error handling system provides a comprehensive solution for monitoring, capturing, and analyzing errors in background processes. By implementing this system, the `realtalk` package will gain significant improvements in stability, debuggability, and user experience during error scenarios.

The three main R6 classes (ManagedProcess, ProcessManager, and Logger) create a cohesive framework for error handling, with direct integration into the main loop for responsive health checks and recovery.
