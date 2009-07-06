-record(tag, {
          sequence,   % string()
          chromosome, % string()
          position,   % integer()
          strand,     % up | down
          length,     % integer()
          repeat,     % true | false
          mmei        % no | {yes, integer()}
          }).
        
