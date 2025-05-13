#' R6 Class for Managing Multiple Background Processes
#'
#' @description
#' The ProcessManager class provides a centralized system for tracking and monitoring
#' multiple background processes. It maintains a registry of ManagedProcess instances,
#' facilitates health checks, and provides error history and recovery functionality.
#'
#' @export
ProcessManager <- R6::R6Class("ProcessManager",
  public = list(
    #' @description
    #' Create a new ProcessManager
    #'
    #' @param logger A Logger instance for logging events
    #'
    #' @return A new ProcessManager object
    initialize = function(logger) {
      # Validate inputs
      checkmate::assert_class(logger, classes = "Logger")
      
      # Store references
      private$logger <- logger
      private$processes <- list()
      
      # Log initialization
      logger$info(
        component = "ProcessManager",
        message = "Process manager initialized"
      )
    },
    
    #' @description
    #' Register a process for management
    #'
    #' @param process A callr process object (typically from callr::r_bg)
    #' @param name A unique identifier string for the process
    #'
    #' @return The registered ManagedProcess instance
    register_process = function(process, name) {
      # Validate inputs
      checkmate::assert_class(process, classes = "process")
      checkmate::assert_string(name, min.chars = 1)
      
      # Check for duplicate name
      if (name %in% names(private$processes)) {
        private$logger$warning(
          component = "ProcessManager",
          message = "Process name already registered, overwriting",
          context = list(name = name)
        )
      }
      
      # Create managed process
      private$logger$info(
        component = "ProcessManager",
        message = "Registering new process",
        context = list(name = name)
      )
      
      managed_process <- ManagedProcess$new(
        process = process,
        name = name,
        logger = private$logger
      )
      
      # Store in registry
      private$processes[[name]] <- managed_process
      
      # Log registration
      private$logger$info(
        component = "ProcessManager",
        message = "Process registered successfully",
        context = list(name = name)
      )
      
      return(managed_process)
    },
    
    #' @description
    #' Check health of all managed processes
    #'
    #' @return A named list of process status information
    check_processes = function() {
      if (length(private$processes) == 0) {
        private$logger$info(
          component = "ProcessManager",
          message = "No processes registered to check"
        )
        return(list())
      }
      
      private$logger$info(
        component = "ProcessManager",
        message = "Checking all processes",
        context = list(count = length(private$processes))
      )
      
      # Check each process and collect status
      statuses <- list()
      for (name in names(private$processes)) {
        statuses[[name]] <- private$processes[[name]]$check()
      }
      
      # Count running and dead processes
      running_count <- sum(sapply(statuses, function(s) s$status == "running"))
      dead_count <- sum(sapply(statuses, function(s) s$status == "dead"))
      
      private$logger$info(
        component = "ProcessManager",
        message = "Process check complete",
        context = list(
          total = length(statuses),
          running = running_count,
          dead = dead_count
        )
      )
      
      return(statuses)
    },
    
    #' @description
    #' Check health of a specific process
    #'
    #' @param name The name of the process to check
    #'
    #' @return Status information for the specified process
    check_process = function(name) {
      # Validate inputs
      checkmate::assert_string(name, min.chars = 1)
      
      if (!name %in% names(private$processes)) {
        private$logger$warning(
          component = "ProcessManager",
          message = "Attempted to check unknown process",
          context = list(name = name)
        )
        cli::cli_abort("Process not found: {name}")
      }
      
      private$logger$info(
        component = "ProcessManager",
        message = "Checking process",
        context = list(name = name)
      )
      
      return(private$processes[[name]]$check())
    },
    
    #' @description
    #' Get error history for one or all processes
    #'
    #' @param name Optional process name; if NULL, returns error history for all processes
    #'
    #' @return A named list containing error information for specified processes
    get_error_history = function(name = NULL) {
      if (!is.null(name)) {
        # Get history for a specific process
        checkmate::assert_string(name, min.chars = 1)
        
        if (!name %in% names(private$processes)) {
          private$logger$warning(
            component = "ProcessManager",
            message = "Attempted to get error history for unknown process",
            context = list(name = name)
          )
          cli::cli_abort("Process not found: {name}")
        }
        
        return(private$processes[[name]]$get_combined_output())
      } else {
        # Get history for all processes
        history <- list()
        for (proc_name in names(private$processes)) {
          history[[proc_name]] <- private$processes[[proc_name]]$get_combined_output()
        }
        return(history)
      }
    },
    
    #' @description
    #' Attempt to restart a dead process
    #'
    #' @param name The name of the process to restart
    #' @param restart_function A function that returns a new process object
    #'
    #' @return TRUE if restart succeeded, FALSE otherwise
    restart_process = function(name, restart_function) {
      # Validate inputs
      checkmate::assert_string(name, min.chars = 1)
      checkmate::assert_function(restart_function)
      
      if (!name %in% names(private$processes)) {
        private$logger$warning(
          component = "ProcessManager",
          message = "Attempted to restart unknown process",
          context = list(name = name)
        )
        cli::cli_abort("Process not found: {name}")
      }
      
      private$logger$info(
        component = "ProcessManager",
        message = "Attempting to restart process",
        context = list(name = name)
      )
      
      # Delegate to the managed process's restart method
      result <- private$processes[[name]]$restart(restart_function)
      
      if (result) {
        private$logger$info(
          component = "ProcessManager",
          message = "Process restart succeeded",
          context = list(name = name)
        )
      } else {
        private$logger$error(
          component = "ProcessManager",
          message = "Process restart failed",
          context = list(name = name)
        )
      }
      
      return(result)
    }
  ),
  
  private = list(
    # Reference to logger
    logger = NULL,
    
    # Registry of managed processes
    processes = NULL
  )
)